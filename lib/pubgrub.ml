open Import

type name = RootName | Name of string [@@deriving repr]
type version = RootVersion | Version of string [@@deriving repr]
type package = name * version [@@deriving repr]
type dependency = package * (name * version list) [@@deriving repr]
type dependencies = (package, name * version list) Hashtbl.t [@@deriving repr]
type available_versions = (name, version) Hashtbl.t [@@deriving repr]
type resolution = package list [@@deriving repr]

let debug_enabled = ref false

let debug_printf fmt =
  if !debug_enabled then Format.printf fmt else Format.ifprintf Format.std_formatter fmt

let set_debug enabled = debug_enabled := enabled

type polarity = Pos | Neg [@@deriving repr]

(* TODO version formula *)
type term = polarity * name * version list [@@deriving repr]

type cause =
  | RootCause
  | NoVersions
  | Dependency of dependency
  | Derived of incompatibility * incompatibility
[@@deriving repr]

and incompatibility = { terms : term list; cause : cause } [@@deriving repr]

type decision_level = int [@@deriving repr]

type assignment = Decision of package | Derivation of term * incompatibility
[@@deriving repr]

type state = {
  incomps : incompatibility list;
  solution : (assignment * decision_level) list;
  decision_level : decision_level;
}

let term_name = function Pos, name, _ | Neg, name, _ -> name

let negate_term = function
  | Pos, name, versions -> (Neg, name, versions)
  | Neg, name, versions -> (Pos, name, versions)

let package_satisfies_term (pkg_name, pkg_version) = function
  | Pos, name, versions -> name = pkg_name && List.mem pkg_version versions
  | Neg, name, versions -> name = pkg_name && not (List.mem pkg_version versions)

let packages_satisfy_term packages term =
  List.exists (fun pkg -> package_satisfies_term pkg term) packages

let packages_contradict_term packages term =
  packages_satisfy_term packages (negate_term term)

let get_assigned_packages solution =
  List.fold_left
    (fun acc (assignment, _decision_level) ->
      match assignment with
      | Decision pkg -> pkg :: acc
      | Derivation ((Pos, name, [ version ]), _) ->
          (* Only count single-version positive derivations as assignments *)
          (name, version) :: acc
      | Derivation ((Pos, _, _), _) ->
          (* Multi-version derivations don't select a specific version *)
          acc
      | Derivation ((Neg, _, _), _) -> acc)
    [] solution

let term_satisfied_by_solution term solution =
  let packages = get_assigned_packages solution in
  packages_satisfy_term packages term

let term_contradicted_by_solution term solution =
  let packages = get_assigned_packages solution in
  packages_contradict_term packages term

let incompatibility_satisfied solution incomp =
  List.for_all (fun term -> term_satisfied_by_solution term solution) incomp.terms

module NameMap = Map.Make (struct
  type t = name

  let compare = compare
end)

let intersect_vs a b = List.filter (fun v -> List.mem v b) a

let normalise_terms terms =
  let tbl = Hashtbl.create (List.length terms) in
  List.iter
    (function
      | Neg, RootName, _ -> ()
      | pol, name, vs -> (
          let replace = Hashtbl.replace tbl name in
          match Hashtbl.find_opt tbl name with
          | None -> replace (pol, vs)
          | Some (pol', vs') -> (
              match (pol, pol') with
              | Pos, Pos | Neg, Neg -> replace (pol, intersect_vs vs vs')
              | Pos, Neg | Neg, Pos -> replace (Pos, if pol = Pos then vs else vs'))))
    terms;
  Hashtbl.fold (fun name (pol, vs) acc -> (pol, name, vs) :: acc) tbl []

let get_unsatisfied_terms solution incomp =
  List.filter
    (fun t ->
      (not (term_satisfied_by_solution t solution))
      && not (term_contradicted_by_solution t solution))
    incomp.terms

let rec conflict_resolution state original_incomp incomp :
    (state * incompatibility * term, incompatibility) Result.t =
  debug_printf "conflict resolution on: %a\n" (Repr.pp incompatibility_t) incomp;
  (* NOTE could this be made more efficient by iterating over terms then assignments (rather than assignments then terms)? *)
  let rec find_earliest_satisfier incomp = function
    | [] -> []
    | assignment :: assignments -> (
        match find_earliest_satisfier incomp assignments with
        | [] ->
            if incompatibility_satisfied (assignment :: assignments) incomp then
              assignment :: assignments
            else []
        | solution -> solution)
  in
  let rec find_previous_satisfier satisfier incomp = function
    | [] -> []
    | assignment :: assignments -> (
        match find_previous_satisfier satisfier incomp assignments with
        | [] ->
            if incompatibility_satisfied (satisfier :: assignment :: assignments) incomp
            then assignment :: assignments
            else []
        | solution -> solution)
  in
  match incomp.terms with
  | [] | [ (Pos, RootName, [ RootVersion ]) ] -> Error incomp
  | _ -> (
      let (satisfier, satisfier_decision_level), assignments =
        match find_earliest_satisfier incomp state.solution with
        | assignment :: assignments -> (assignment, assignments)
        | _ -> failwith "Incompatibility not satisfied"
      in
      debug_printf "satisfiying assignment on level %d: %a\n" satisfier_decision_level
        (Repr.pp assignment_t) satisfier;
      (* NOTE we could possibly do this inline *)
      let term =
        let name =
          match satisfier with
          | Decision (name, _) -> name
          | Derivation ((_, name, _), _) -> name
        in
        List.find (fun t -> term_name t = name) incomp.terms
      in
      let previous_satisfier_level =
        match
          find_previous_satisfier (satisfier, satisfier_decision_level) incomp assignments
        with
        | (_, decision_level) :: _ -> decision_level
        | _ -> 1
      in
      match (satisfier, satisfier_decision_level != previous_satisfier_level) with
      | Decision _, _ | _, true ->
          debug_printf "backtracking to level %d\n" previous_satisfier_level;
          let solution =
            List.filter
              (fun (_assignment, decision_level) ->
                decision_level <= previous_satisfier_level)
              state.solution
          in
          let incomps =
            if incomp != original_incomp then (
              debug_printf "new incompatibility %a\n" (Repr.pp incompatibility_t) incomp;
              incomp :: state.incomps)
            else state.incomps
          in
          let state = { incomps; solution; decision_level = previous_satisfier_level } in
          Ok (state, incomp, term)
      | Derivation (_, cause), _ ->
          let prior_cause =
            {
              terms =
                incomp.terms @ cause.terms
                |> List.filter (fun t -> term_name t <> term_name term)
                |> normalise_terms;
              cause = Derived (incomp, cause);
            }
          in
          debug_printf "prior cause %a\n" (Repr.pp incompatibility_t) prior_cause;
          conflict_resolution state original_incomp prior_cause)

let rec unit_propagation state changed : (state, incompatibility) Result.t =
  match changed with
  | [] -> Ok state
  | name :: changed ->
      debug_printf "unit propiagation on: %a\n" (Repr.pp name_t) name;
      (* incompatibilities that refer to `name` *)
      (* NOTE could add a `(name, incompatibilty) Hashtbl.t` for fast access *)
      let incomps =
        let term_names terms = List.map term_name terms in
        List.filter (fun incomp -> List.mem name (term_names incomp.terms)) state.incomps
      in
      incompat_propagation state name changed incomps

and incompat_propagation state name changed = function
  | [] -> unit_propagation state changed
  | incomp :: incomps -> (
      if incompatibility_satisfied state.solution incomp then
        match conflict_resolution state incomp incomp with
        | Ok (state, incomp, term) ->
            let assignment = Derivation (negate_term term, incomp) in
            debug_printf "new assignment on level %d: %a\n" state.decision_level
              (Repr.pp assignment_t) assignment;
            let state =
              {
                state with
                solution = (assignment, state.decision_level) :: state.solution;
              }
            in
            incompat_propagation state name [ name ] incomps
        | Error incomp -> Error incomp
      else
        match get_unsatisfied_terms state.solution incomp with
        | [ term ] ->
            let assignment = Derivation (negate_term term, incomp) in
            debug_printf "new assignment on level %d: %a\n" state.decision_level
              (Repr.pp assignment_t) assignment;
            let state =
              {
                state with
                solution = (assignment, state.decision_level) :: state.solution;
              }
            in
            let _, name, _ = term in
            incompat_propagation state name (name :: changed) incomps
        | _ -> incompat_propagation state name changed incomps)

let make_decision available_versions dependencies state =
  let open Option.Syntax in
  (* Find a name that has positive derivations but no decision and collect it's versions *)
  (* NOTE could index over this *)
  let rec find_undecided_term = function
    | [] -> None
    | (Derivation ((Pos, name, versions), _), _) :: assignments ->
        let rec find_versions = function
          | [] -> Some versions
          | (Decision (n, _), _) :: _ when n = name -> None
          | (Derivation ((Pos, n, vs), _), _) :: assignments when n = name ->
              (* Filter to only versions that are actually available *)
              let* vs' = find_versions assignments in
              Some (intersect_vs vs vs')
          | _ :: assignments -> find_versions assignments
        in
        let* vs = find_versions assignments in
        Some (name, vs)
    | solution -> find_undecided_term solution
  in
  let* name, vs = find_undecided_term state.solution in
  let vs = intersect_vs (Hashtbl.find_all available_versions name) vs in
  debug_printf "deciding on %a\n" (Repr.pp name_t) name;
  (* TODO prioritise versions *)
  match vs with
  | [] ->
      let incomp = { terms = [ (Pos, name, vs) ]; cause = NoVersions } in
      debug_printf "no versions found, adding incompatiblity %a\n"
        (Repr.pp incompatibility_t) incomp;
      let state = { state with incomps = incomp :: state.incomps } in
      Some (name, state)
  | version :: _ ->
      debug_printf "trying version %a\n" (Repr.pp version_t) version;
      let dep_incomps =
        List.map
          (fun (dep_name, dep_versions) ->
            {
              terms =
                [
                  (* If this package is selected, *)
                  (Pos, name, [ version ]);
                  (* then we can't not have a compatible dependency *)
                  (Neg, dep_name, dep_versions);
                ];
              cause = Dependency ((name, version), (dep_name, dep_versions));
            })
          (Hashtbl.find_all dependencies (name, version))
      in
      debug_printf "added dependency incompatibilities %a\n"
        (Repr.pp (Repr.list incompatibility_t))
        dep_incomps;
      let incomps = dep_incomps @ state.incomps in
      let solution, decision_level =
        let decision_level = state.decision_level + 1 in
        let assignment = Decision (name, version) in
        let solution = (assignment, decision_level) :: state.solution in
        match List.find_opt (incompatibility_satisfied solution) incomps with
        | Some incomp ->
            debug_printf "not adding due to incompatibility %a\n"
              (Repr.pp incompatibility_t) incomp;
            (state.solution, state.decision_level)
        | None ->
            debug_printf "adding decision %a\n" (Repr.pp assignment_t) assignment;
            (solution, decision_level)
      in
      let state = { incomps; solution; decision_level } in
      Some (name, state)

let extract_resolution state =
  List.filter_map
    (function
      | assignment, _decision_level -> (
          match assignment with Decision pkg -> Some pkg | _ -> None))
    state.solution

let init_incomps dependencies =
  { terms = [ (Neg, RootName, [ RootVersion ]) ]; cause = RootCause }
  :: List.map
       (fun ((name, versions) as dep) ->
         {
           terms =
             [
               (* If the root is selected, *)
               (Pos, RootName, [ RootVersion ]);
               (* then we can't not have a compatible dependency *)
               (Neg, name, versions);
             ];
           cause = Dependency ((RootName, RootVersion), dep);
         })
       (Hashtbl.find_all dependencies (RootName, RootVersion))

let solve (available_versions : available_versions) (dependencies : dependencies) :
    (package list, incompatibility) Result.t =
  let rec solve_loop state next =
    match unit_propagation state [ next ] with
    | Error incomp -> Error incomp
    | Ok state -> (
        match make_decision available_versions dependencies state with
        | None -> Ok (extract_resolution state)
        | Some (next, state) -> solve_loop state next)
  in
  let incomps = init_incomps dependencies in
  debug_printf "initial incompatibilities %a\n"
    (Repr.pp (Repr.list incompatibility_t))
    incomps;
  solve_loop { incomps; solution = []; decision_level = 1 } RootName

(* TODO *)
let rec explain_incompatibility fmt incompat =
  match incompat.cause with
  | RootCause -> Format.fprintf fmt "root"
  | Dependency dependency -> Format.fprintf fmt "Dep %a" (Repr.pp dependency_t) dependency
  | NoVersions ->
      Format.fprintf fmt "%a not available" (Repr.pp (Repr.list term_t)) incompat.terms
  | Derived (cause1, cause2) ->
      Format.fprintf fmt "(%a && %a)" explain_incompatibility cause1
        explain_incompatibility cause2
