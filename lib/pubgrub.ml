open Import
open Option.Syntax
open Result.Syntax

type name = RootName | Name of string [@@deriving repr]
type version = RootVersion | Version of string [@@deriving repr]
type package = name * version [@@deriving repr]
type dependency = package * (name * version list) [@@deriving repr]
type dependencies = (package, name * version list) Hashtbl.t [@@deriving repr]
type available_versions = (name, version) Hashtbl.t [@@deriving repr]
type resolution = package list [@@deriving repr]
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

type solution = (assignment * decision_level) list [@@deriving repr]

type state = {
  incomps : incompatibility list;
  solution : solution;
  decision_level : decision_level;
}
[@@deriving repr]

let name_t =
  let pp fmt = function
    | RootName -> Format.pp_print_string fmt "Root"
    | Name n -> Format.pp_print_string fmt n
  in
  Repr.like ~pp name_t

let version_t =
  let pp fmt = function
    | RootVersion -> Format.pp_print_string fmt "Root"
    | Version n -> Format.pp_print_string fmt n
  in
  Repr.like ~pp version_t

let versions_t =
  let pp fmt vs =
    Format.fprintf fmt "(%a)"
      Format.(
        pp_print_list
          ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
          (fun fmt v -> fprintf fmt "%a" (Repr.pp version_t) v))
      vs
  in
  Repr.like ~pp (Repr.list version_t)

let package_t =
  let pp fmt (n, v) =
    Format.fprintf fmt "%a %a" (Repr.pp name_t) n (Repr.pp version_t) v
  in
  Repr.like ~pp package_t

let dependency_t =
  let pp fmt (p, (n, vs)) =
    Format.fprintf fmt "%a -> %a %a" (Repr.pp package_t) p (Repr.pp name_t) n
      Repr.(pp versions_t)
      vs
  in
  Repr.like ~pp dependency_t

let resolution_t =
  let pp =
    Format.(
      pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        (fun fmt (n, v) -> fprintf fmt "%a %a" (Repr.pp name_t) n (Repr.pp version_t) v))
  in
  Repr.like ~pp resolution_t

let polarity_t =
  let pp fmt = function
    | Pos -> Format.fprintf fmt "%s" ""
    | Neg -> Format.fprintf fmt "%s" "not "
  in
  Repr.like ~pp polarity_t

let term_t =
  let pp fmt (p, n, vs) =
    Format.fprintf fmt "%a%a %a" (Repr.pp polarity_t) p (Repr.pp name_t) n
      Repr.(pp versions_t)
      vs
  in
  Repr.like ~pp term_t

let terms_t =
  let pp fmt terms =
    Format.fprintf fmt "{%a}"
      Format.(
        pp_print_list
          ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
          (fun fmt t -> fprintf fmt "%a" (Repr.pp term_t) t))
      terms
  in
  Repr.like ~pp (Repr.list term_t)

(* TODO proper recursion *)
let incompatibility_t =
  let pp fmt { terms; cause } =
    Format.fprintf fmt "(terms: %a, cause: %a)" (Repr.pp terms_t) terms
      (Repr.pp
         (let pp fmt = function
            | RootCause -> Format.pp_print_string fmt "root"
            | NoVersions -> Format.pp_print_string fmt "no versions"
            | Dependency dep ->
                Format.fprintf fmt "dependency %a" (Repr.pp dependency_t) dep
            | Derived (i1, i2) ->
                Format.fprintf fmt "(%a and %a)" (Repr.pp incompatibility_t) i1
                  (Repr.pp incompatibility_t) i2
          in
          Repr.like ~pp cause_t))
      cause
  in
  Repr.like ~pp incompatibility_t

let incompatibilities_t =
  let pp fmt incomps =
    Format.fprintf fmt "%a"
      Format.(
        pp_print_list
          ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "\n\t")
          (fun fmt i -> fprintf fmt "%a" (Repr.pp incompatibility_t) i))
      incomps
  in
  Repr.like ~pp (Repr.list incompatibility_t)

let assignment_t =
  let pp fmt = function
    | Decision package -> Format.fprintf fmt "Decision %a" (Repr.pp package_t) package
    | Derivation (term, cause) ->
        Format.fprintf fmt "Derivation %a due to incompatibility %a" (Repr.pp term_t) term
          (Repr.pp incompatibility_t) cause
  in
  Repr.like ~pp assignment_t

let _solution_t =
  let pp fmt =
    Format.(
      pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        (fun fmt (a, d) -> fprintf fmt "(%d: %a)" d (Repr.pp assignment_t) a))
      fmt
  in
  Repr.like ~pp solution_t

let debug_enabled = ref false

let debug_printf fmt =
  if !debug_enabled then
    Format.kfprintf
      (fun _ -> Format.pp_print_flush Format.std_formatter ())
      Format.std_formatter fmt
  else Format.ifprintf Format.std_formatter fmt

let set_debug enabled = debug_enabled := enabled
let intersect a b = List.filter (fun e -> List.mem e b) a
let subset a b = List.for_all (fun e -> List.mem e b) a
let disjoint a b = not (List.exists (fun e -> List.mem e b) a)
let minus a b = List.filter (fun e -> not (List.mem e b)) a
let term_name = function Pos, name, _ | Neg, name, _ -> name

let negate_term = function
  | Pos, name, versions -> (Neg, name, versions)
  | Neg, name, versions -> (Pos, name, versions)

module NameMap = Map.Make (struct
  type t = name

  let compare = compare
end)

let term_satisfied_by_solution term solution =
  let rec solution_versions name = function
    | [] -> (None, None)
    | (Decision (n, v), _) :: _ when n = name -> (Some [ v ], None)
    | (Derivation ((Pos, n, pvs), _), _) :: solution when n = name -> (
        match solution_versions name solution with
        | None, nvs -> (Some pvs, nvs)
        | Some pvs', nvs -> (Some (intersect pvs pvs'), nvs))
    | (Derivation ((Neg, n, nvs), _), _) :: solution when n = name -> (
        match solution_versions name solution with
        | pvs, None -> (pvs, Some nvs)
        | pvs', Some nvs' -> (pvs', Some (intersect nvs nvs')))
    | _ :: solution -> solution_versions name solution
  in
  (* find compatible versions of name in solution *)
  let pol, name, vs = term in
  match solution_versions name solution with
  | None, None -> false
  | None, Some nvs' -> ( match pol with Neg -> subset nvs' vs | Pos -> false)
  | Some pvs', nvs' -> (
      let vs' = match nvs' with Some nvs' -> minus pvs' nvs' | None -> pvs' in
      match pol with Pos -> subset vs' vs | Neg -> disjoint vs' vs)

let term_contradicted_by_solution term solution =
  term_satisfied_by_solution (negate_term term) solution

let incompatibility_satisfied solution incomp =
  List.for_all (fun term -> term_satisfied_by_solution term solution) incomp.terms

let get_unsatisfied_terms solution incomp =
  List.filter
    (fun t ->
      (not (term_satisfied_by_solution t solution))
      && not (term_contradicted_by_solution t solution))
    incomp.terms

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
              | Pos, Pos | Neg, Neg -> replace (pol, intersect vs vs')
              | Pos, Neg | Neg, Pos -> replace (Pos, if pol = Pos then vs else vs'))))
    terms;
  Hashtbl.fold (fun name (pol, vs) acc -> (pol, name, vs) :: acc) tbl []

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
      debug_printf "unit propagation on: %a\n" (Repr.pp name_t) name;
      (* incompatibilities that refer to `name` *)
      (* NOTE could add a `(name, incompatibilty) Hashtbl.t` for fast access *)
      let incomps =
        let term_names terms = List.map term_name terms in
        List.filter (fun incomp -> List.mem name (term_names incomp.terms)) state.incomps
      in
      incompat_propagation state changed incomps

and incompat_propagation state changed = function
  | [] -> unit_propagation state changed
  | incomp :: incomps -> (
      if incompatibility_satisfied state.solution incomp then
        match conflict_resolution state incomp incomp with
        | Ok (state, incomp, term) ->
            let assignment = Derivation (negate_term term, incomp) in
            let _, name, _ = term in
            debug_printf "new assignment on level %d: %a\n" state.decision_level
              (Repr.pp assignment_t) assignment;
            let state =
              {
                state with
                solution = (assignment, state.decision_level) :: state.solution;
              }
            in
            unit_propagation state [ name ]
        | Error incomp -> Error incomp
      else
        match get_unsatisfied_terms state.solution incomp with
        | [ term ] ->
            let assignment = Derivation (negate_term term, incomp) in
            debug_printf "new assignment on level %d: %a\n" state.decision_level
              (Repr.pp assignment_t) assignment;
            let solution = (assignment, state.decision_level) :: state.solution in
            let state = { state with solution } in
            let _, name, _ = term in
            incompat_propagation state (name :: changed) incomps
        | _ -> incompat_propagation state changed incomps)

let dependency_incomps dependencies (name, version) =
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

let make_decision available_versions dependencies state =
  (* Find a name that has positive derivations but no decision and collect it's versions *)
  (* NOTE could index over this *)
  let rec find_versions name = function
    | [] -> Ok None
    | (Decision (n, _), _) :: _ when n = name -> Error None
    | (Derivation ((Pos, n, pvs), _), _) :: assignments when n = name -> (
        let> r = find_versions name assignments in
        match r with
        | None -> Ok (Some (pvs, []))
        | Some (pvs', nvs') -> Ok (Some (intersect pvs pvs', nvs')))
    (* NOTE is this correct? *)
    | (Derivation ((Neg, n, nvs), _), _) :: assignments when n = name -> (
        let> r = find_versions name assignments in
        match r with
        | None -> Ok (Some ([], nvs))
        | Some (pvs', nvs') -> Ok (Some (pvs', intersect nvs nvs')))
    | _ :: assignments -> find_versions name assignments
  in
  let rec find_undecided_term = function
    | [] -> None
    | (Derivation ((Pos, name, _), _), _) :: solution when name != RootName -> (
        match find_versions name state.solution with
        | Ok (Some (pvs, nvs)) ->
            let vs = minus pvs nvs in
            Some (name, vs)
        | _ -> find_undecided_term solution)
    | _ :: solution -> find_undecided_term solution
  in
  let* name, vs = find_undecided_term state.solution in
  debug_printf "deciding on %a: %a\n" (Repr.pp name_t) name (Repr.pp versions_t) vs;
  let decision_level = state.decision_level + 1 in
  (* Filter to only versions that are actually available *)
  let real_vs = intersect (Hashtbl.find_all available_versions name) vs in
  match real_vs with
  | [] ->
      let incomp = { terms = [ (Pos, name, vs) ]; cause = NoVersions } in
      debug_printf "no versions found, adding incompatiblity %a\n"
        (Repr.pp incompatibility_t) incomp;
      let state = { state with incomps = incomp :: state.incomps } in
      Some (name, state)
  | _ ->
      let rec try_versions state = function
        | [] -> Some (name, state)
        | version :: versions -> (
            debug_printf "trying version %a\n" (Repr.pp version_t) version;
            let dep_incomps = dependency_incomps dependencies (name, version) in
            if List.length dep_incomps > 0 then
              debug_printf "dependency incompatibilities\n\t%a\n"
                Repr.(pp incompatibilities_t)
                dep_incomps;
            let incomps = dep_incomps @ state.incomps in
            let state = { state with incomps } in
            let assignment = Decision (name, version) in
            let solution = (assignment, decision_level) :: state.solution in
            match List.find_opt (incompatibility_satisfied solution) incomps with
            | Some incomp ->
                debug_printf "not adding due to incompatibility %a\n"
                  (Repr.pp incompatibility_t) incomp;
                try_versions state versions
            | None ->
                debug_printf "assignment on level %d: %a\n" decision_level
                  (Repr.pp assignment_t) assignment;
                let state = { incomps; solution; decision_level } in
                Some (name, state))
      in
      (* TODO prioritise versions *)
      try_versions state (List.sort (fun a b -> compare b a) real_vs)

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
  debug_printf "initial incompatibilities\n\t%a\n" Repr.(pp incompatibilities_t) incomps;
  solve_loop { incomps; solution = []; decision_level = 0 } RootName

(* TODO *)
let rec explain_incompatibility fmt incompat =
  match incompat.cause with
  | RootCause -> Format.fprintf fmt "root"
  | Dependency dependency -> Format.fprintf fmt "Dep %a" (Repr.pp dependency_t) dependency
  | NoVersions -> Format.fprintf fmt "%a not available" Repr.(pp terms_t) incompat.terms
  | Derived (cause1, cause2) ->
      Format.fprintf fmt "(%a && %a)" explain_incompatibility cause1
        explain_incompatibility cause2
