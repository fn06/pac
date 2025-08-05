open Import
open Option.Syntax
open Result.Syntax

type name = RootName | Name of string
type version = RootVersion | Version of string
type package = name * version
type dependency = package * (name * version list)
type dependencies = (package, name * version list) Hashtbl.t
type available_versions = (name, version) Hashtbl.t
type resolution = package list
type polarity = Pos | Neg

(* TODO version formula *)
type term = polarity * name * version list

type cause =
  | RootCause
  | NoVersions
  | Dependency of dependency
  | Derived of incompatibility * incompatibility

and incompatibility = { terms : term list; cause : cause }

type decision_level = int
type assignment = Decision of package | Derivation of term * incompatibility
type solution = (assignment * decision_level) list

type state = {
  incomps : incompatibility list;
  solution : solution;
  decision_level : decision_level;
}

let pp_name fmt = function
  | RootName -> Format.pp_print_string fmt "Root"
  | Name n -> Format.pp_print_string fmt n

let pp_version fmt = function
  | RootVersion -> Format.pp_print_string fmt "Root"
  | Version n -> Format.pp_print_string fmt n

let pp_versions fmt vs =
  Format.fprintf fmt "(%a)"
    Format.(
      pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        (fun fmt v -> fprintf fmt "%a" pp_version v))
    vs

let pp_package fmt (n, v) = Format.fprintf fmt "%a %a" pp_name n pp_version v

let pp_dependency fmt (p, (n, vs)) =
  Format.fprintf fmt "%a -> %a %a" pp_package p pp_name n pp_versions vs

let pp_resolution =
  Format.(
    pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
      (fun fmt (n, v) -> fprintf fmt "%a %a" pp_name n pp_version v))

let pp_polarity fmt = function
  | Pos -> Format.fprintf fmt "%s" ""
  | Neg -> Format.fprintf fmt "%s" "not "

let pp_term fmt (p, n, vs) =
  Format.fprintf fmt "%a%a %a" pp_polarity p pp_name n pp_versions vs

let pp_terms fmt terms =
  Format.fprintf fmt "{%a}"
    Format.(
      pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        (fun fmt t -> fprintf fmt "%a" pp_term t))
    terms

let rec pp_cause fmt = function
  | RootCause -> Format.pp_print_string fmt "root"
  | NoVersions -> Format.pp_print_string fmt "no versions"
  | Dependency dep -> Format.fprintf fmt "dependency %a" pp_dependency dep
  | Derived (i1, i2) ->
      Format.fprintf fmt "(%a and %a)" pp_incompatibility i1 pp_incompatibility i2

and pp_incompatibility fmt { terms; cause } =
  Format.fprintf fmt "(terms: %a, cause: %a)" pp_terms terms pp_cause cause

let pp_incompatibilities fmt incomps =
  Format.fprintf fmt "%a"
    Format.(
      pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "\n\t")
        (fun fmt i -> fprintf fmt "%a" pp_incompatibility i))
    incomps

let pp_assignment fmt = function
  | Decision package -> Format.fprintf fmt "Decision %a" pp_package package
  | Derivation (term, cause) ->
      Format.fprintf fmt "Derivation %a due to incompatibility %a" pp_term term
        pp_incompatibility cause

let _pp_solution fmt =
  Format.(
    pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
      (fun fmt (a, d) -> fprintf fmt "(%d: %a)" d pp_assignment a))
    fmt

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

let get_almost_satisfied_term solution incomp =
  match
    List.fold_left
      (fun acc t ->
        let> sat = acc in
        if term_contradicted_by_solution t solution then Error None
        else
          match (sat, term_satisfied_by_solution t solution) with
          | None, false -> Ok (Some t)
          | Some _, false -> Error None
          | Some sat, true -> Ok (Some sat)
          | None, true -> Ok None)
      (Ok None) incomp.terms
  with
  | Ok (Some t) -> Some t
  | _ -> None

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
  debug_printf "conflict resolution on: %a\n" pp_incompatibility incomp;
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
        pp_assignment satisfier;
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
              debug_printf "new incompatibility %a\n" pp_incompatibility incomp;
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
          debug_printf "prior cause %a\n" pp_incompatibility prior_cause;
          conflict_resolution state original_incomp prior_cause)

let rec unit_propagation state changed : (state, incompatibility) Result.t =
  match changed with
  | [] -> Ok state
  | name :: changed ->
      debug_printf "unit propagation on: %a\n" pp_name name;
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
              pp_assignment assignment;
            let state =
              {
                state with
                solution = (assignment, state.decision_level) :: state.solution;
              }
            in
            unit_propagation state [ name ]
        | Error incomp -> Error incomp
      else
        match get_almost_satisfied_term state.solution incomp with
        | Some term ->
            let assignment = Derivation (negate_term term, incomp) in
            debug_printf "new assignment on level %d: %a\n" state.decision_level
              pp_assignment assignment;
            let solution = (assignment, state.decision_level) :: state.solution in
            let state = { state with solution } in
            let _, name, _ = term in
            incompat_propagation state (name :: changed) incomps
        | None -> incompat_propagation state changed incomps)

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
  debug_printf "deciding on %a: %a\n" pp_name name pp_versions vs;
  let decision_level = state.decision_level + 1 in
  (* Filter to only versions that are actually available *)
  let real_vs = intersect (Hashtbl.find_all available_versions name) vs in
  match real_vs with
  | [] ->
      let incomp = { terms = [ (Pos, name, vs) ]; cause = NoVersions } in
      debug_printf "no versions found, adding incompatiblity %a\n" pp_incompatibility
        incomp;
      let state = { state with incomps = incomp :: state.incomps } in
      Some (name, state)
  | _ ->
      let rec try_versions state = function
        | [] -> Some (name, state)
        | version :: versions -> (
            debug_printf "trying version %a\n" pp_version version;
            let dep_incomps = dependency_incomps dependencies (name, version) in
            if List.length dep_incomps > 0 then
              debug_printf "dependency incompatibilities\n\t%a\n" pp_incompatibilities
                dep_incomps;
            let incomps = dep_incomps @ state.incomps in
            let state = { state with incomps } in
            let assignment = Decision (name, version) in
            let solution = (assignment, decision_level) :: state.solution in
            match List.find_opt (incompatibility_satisfied solution) incomps with
            | Some incomp ->
                debug_printf "not adding due to incompatibility %a\n" pp_incompatibility
                  incomp;
                try_versions state versions
            | None ->
                debug_printf "assignment on level %d: %a\n" decision_level pp_assignment
                  assignment;
                let state = { incomps; solution; decision_level } in
                Some (name, state))
      in
      (* TODO version ordering *)
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
  debug_printf "initial incompatibilities\n\t%a\n" pp_incompatibilities incomps;
  solve_loop { incomps; solution = []; decision_level = 0 } RootName

(* TODO *)
let rec explain_incompatibility fmt incompat =
  match incompat.cause with
  | RootCause -> Format.fprintf fmt "root"
  | Dependency dependency -> Format.fprintf fmt "Dep %a" pp_dependency dependency
  | NoVersions -> Format.fprintf fmt "%a not available" pp_terms incompat.terms
  | Derived (cause1, cause2) ->
      Format.fprintf fmt "(%a && %a)" explain_incompatibility cause1
        explain_incompatibility cause2
