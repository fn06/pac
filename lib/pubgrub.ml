(* Core types from the Package Calculus *)
type name = string
type version = string
type package = name * version
type dependency = package * (name * version list)
type dependencies = dependency list
type repository = package list
type query = package list

(* Term represents a statement about a package that may be true or false *)
type term =
  | Positive of name * version list (* package name with allowed versions *)
  | Negative of name * version list (* package name with forbidden versions *)

(* Incompatibility cause tracking for error reporting *)
type external_cause =
  | Dependency of package * package * string list (* depender depends on target with versions *)
  | Unavailable of package (* package version not available *)
  | Custom of string (* custom constraint *)

type cause =
  | External of external_cause (* External fact with structured data *)
  | Derived of
      incompatibility
      * incompatibility (* Derived from two other incompatibilities *)

(* Incompatibility is a set of terms that cannot all be true *)
and incompatibility = {
  terms : term list;
  cause : cause;
  id : int; (* Unique identifier for tracking *)
}

(* Assignment types in partial solution *)
type assignment =
  | Decision of package * int (* (package, decision_level) *)
  | Derivation of
      term * incompatibility * int (* (term, cause, decision_level) *)

(* Partial solution maintains current state *)
type partial_solution = { assignments : assignment list; decision_level : int }

(* PubGrub solver state *)
type solver_state = {
  incompatibilities : incompatibility list;
  partial_solution : partial_solution;
  next_id : int;
}

(* Error types *)
type solve_error =
  | NoSolution of incompatibility (* Root cause incompatibility *)
  | InvalidInput of string

type solve_result = Solution of package list | Error of solve_error

(* Internal result type for intermediate operations *)
type 'a internal_result = Ok of 'a | Error of solve_error

(* Helper functions *)
let rec list_take n = function
  | [] -> []
  | x :: xs when n > 0 -> x :: list_take (n - 1) xs
  | _ -> []

(* Helper functions for terms *)
let term_package = function
  | Positive (name, _) -> name
  | Negative (name, _) -> name

let term_versions = function
  | Positive (_, versions) -> versions
  | Negative (_, versions) -> versions

let is_positive = function Positive _ -> true | Negative _ -> false

let negate_term = function
  | Positive (name, versions) -> Negative (name, versions)
  | Negative (name, versions) -> Positive (name, versions)

(* Check if a package satisfies a term *)
let package_satisfies_term (pkg_name, pkg_version) = function
  | Positive (name, versions) ->
      name = pkg_name && List.mem pkg_version versions
  | Negative (name, versions) ->
      name = pkg_name && not (List.mem pkg_version versions)

(* Check if a set of packages satisfies a term *)
let packages_satisfy_term packages term =
  List.exists (fun pkg -> package_satisfies_term pkg term) packages

(* Check if a set of packages contradicts a term *)
let packages_contradict_term packages term =
  packages_satisfy_term packages (negate_term term)

(* Get all packages currently assigned in partial solution *)
let get_assigned_packages partial_solution =
  List.fold_left
    (fun acc -> function
      | Decision (pkg, _) -> pkg :: acc
      | Derivation (Positive (name, versions), _, _) -> (
          (* For positive derivations, we need to find which specific version was chosen *)
          (* For now, just take the first version as a simplification *)
          match versions with
          | [] -> acc
          | v :: _ -> (name, v) :: acc)
      | Derivation (Negative _, _, _) -> acc)
    [] partial_solution.assignments

(* Check if term is satisfied by current partial solution *)
let term_satisfied_by_partial_solution term partial_solution =
  let packages = get_assigned_packages partial_solution in
  packages_satisfy_term packages term

(* Check if term is contradicted by current partial solution *)
let term_contradicted_by_partial_solution term partial_solution =
  let packages = get_assigned_packages partial_solution in
  packages_contradict_term packages term

(* Check if incompatibility is satisfied by partial solution *)
let incompatibility_satisfied partial_solution incomp =
  List.for_all
    (fun term -> term_satisfied_by_partial_solution term partial_solution)
    incomp.terms

(* Check if incompatibility is almost satisfied (all but one term satisfied) *)
let incompatibility_almost_satisfied partial_solution incomp =
  let satisfied_terms =
    List.filter
      (fun term -> term_satisfied_by_partial_solution term partial_solution)
      incomp.terms
  in
  let unsatisfied_terms =
    List.filter
      (fun t ->
        (not (term_satisfied_by_partial_solution t partial_solution))
        && not (term_contradicted_by_partial_solution t partial_solution))
      incomp.terms
  in
  List.length satisfied_terms = List.length incomp.terms - 1
  && List.length unsatisfied_terms = 1

(* Get the unsatisfied term from an almost-satisfied incompatibility *)
let get_unsatisfied_term partial_solution incomp =
  List.find
    (fun t ->
      (not (term_satisfied_by_partial_solution t partial_solution))
      && not (term_contradicted_by_partial_solution t partial_solution))
    incomp.terms

(* Add assignment to partial solution *)
let add_assignment assignment partial_solution =
  {
    partial_solution with
    assignments = assignment :: partial_solution.assignments;
  }

(* Get packages mentioned in a term *)
let term_packages = function
  | Positive (name, _) | Negative (name, _) -> [ name ]

(* Get all packages mentioned in incompatibility *)
let incompatibility_packages incomp =
  List.concat_map term_packages incomp.terms |> List.sort_uniq String.compare

(* Convert Core dependencies to PubGrub incompatibilities *)
let dependency_to_incompatibility id (depender, (target_name, target_versions))
    =
  {
    terms =
      [
        Positive (fst depender, [ snd depender ]);
        (* If this package is selected *)
        Negative (target_name, target_versions)
        (* Then target must NOT be these versions *);
      ];
    cause = External (Dependency (depender, (target_name, ""), target_versions));
    id;
  }

(* Convert Core dependencies to incompatibilities *)
let dependencies_to_incompatibilities (deps : dependencies) =
  let rec convert acc id = function
    | [] -> List.rev acc
    | dep :: rest ->
        let incomp = dependency_to_incompatibility id dep in
        convert (incomp :: acc) (id + 1) rest
  in
  convert [] 0 deps

(* Create initial solver state *)
let create_initial_state (repo : repository) (deps : dependencies)
    (query : query) =
  let dep_incomps = dependencies_to_incompatibilities deps in
  let next_id = List.length dep_incomps in

  (* Check that all query packages are available *)
  List.iter
    (fun query_pkg ->
      if not (List.mem query_pkg repo) then
        failwith
          (Printf.sprintf "Query package %s %s is not available" (fst query_pkg)
             (snd query_pkg)))
    query;

  (* Add query requirements by starting with them in the partial solution *)
  let query_assignments =
    List.map (fun (name, version) -> Decision ((name, version), 0)) query
  in

  let initial_partial_solution =
    { assignments = query_assignments; decision_level = 0 }
  in

  {
    incompatibilities = dep_incomps;
    partial_solution = initial_partial_solution;
    next_id;
  }

(* Unit Propagation Algorithm *)
let rec unit_propagation (repo : repository) state package_names :
    solver_state internal_result =
  match package_names with
  | [] -> Ok state
  | package_name :: rest -> (
      let relevant_incomps =
        List.filter
          (fun incomp ->
            List.mem package_name (incompatibility_packages incomp))
          state.incompatibilities
      in

      match process_incompatibilities repo state relevant_incomps with
      | Ok new_state ->
          let changed_packages =
            if new_state != state then [ package_name ] else []
          in
          unit_propagation repo new_state (rest @ changed_packages)
      | Error err -> Error err)

and process_incompatibilities (repo : repository) state incomps :
    solver_state internal_result =
  match incomps with
  | [] -> Ok state
  | incomp :: rest ->
      if incompatibility_satisfied state.partial_solution incomp then
        (* Conflict detected - need conflict resolution *)
        match conflict_resolution state incomp with
        | Ok new_state ->
            Ok new_state (* Continue with learned incompatibility *)
        | Error err -> Error err
      else if incompatibility_almost_satisfied state.partial_solution incomp
      then
        (* Unit propagation possible *)
        let unsatisfied_term =
          get_unsatisfied_term state.partial_solution incomp
        in
        let negated_term = negate_term unsatisfied_term in

        (* Check if positive derivation requires unavailable packages *)
        match negated_term with
        | Positive (name, versions) ->
            let available_versions =
              List.filter
                (fun version -> List.mem (name, version) repo)
                versions
            in
            if List.length available_versions = 0 then
              (* Create incompatibilities showing the full chain: dependency + unavailability *)
              let unavailable_incomp = {
                terms = [Positive (name, versions)];
                cause = External (Unavailable (name, String.concat " or " versions));
                id = state.next_id;
              } in
              let derived_incomp = {
                terms = List.filter (fun t -> term_package t <> name) incomp.terms;
                cause = Derived (incomp, unavailable_incomp);
                id = state.next_id + 1;
              } in
              Error (NoSolution derived_incomp)
            else
              let new_assignment =
                Derivation
                  (negated_term, incomp, state.partial_solution.decision_level)
              in
              let new_partial_solution =
                add_assignment new_assignment state.partial_solution
              in
              let new_state =
                { state with partial_solution = new_partial_solution }
              in
              process_incompatibilities repo new_state rest
        | Negative _ ->
            let new_assignment =
              Derivation
                (negated_term, incomp, state.partial_solution.decision_level)
            in
            let new_partial_solution =
              add_assignment new_assignment state.partial_solution
            in
            let new_state =
              { state with partial_solution = new_partial_solution }
            in
            process_incompatibilities repo new_state rest
      else process_incompatibilities repo state rest

(* Conflict Resolution Algorithm *)
and conflict_resolution state conflicting_incomp : solver_state internal_result
    =
  let rec resolve_conflict incomp =
    (* Check if this is a root-level failure: empty terms or contains query packages *)
    if List.length incomp.terms = 0 then Error (NoSolution incomp)
    else
      (* Find the earliest assignment that makes the incompatibility satisfied *)
      match
        find_earliest_satisfying_assignment state.partial_solution incomp
      with
      | None -> Error (NoSolution incomp)
      | Some (satisfier, satisfier_term) -> (
          (* Find previous satisfier *)
          match
            find_previous_satisfier_for_incomp state.partial_solution incomp
              satisfier
          with
          | None ->
              (* No previous satisfier - backtrack to decision level 1 *)
              let previous_satisfier_level = 1 in
              let new_assignments =
                List.filter
                  (fun assignment ->
                    get_assignment_level assignment <= previous_satisfier_level)
                  state.partial_solution.assignments
              in
              let new_partial_solution =
                {
                  assignments = new_assignments;
                  decision_level = previous_satisfier_level;
                }
              in
              let new_state =
                {
                  incompatibilities = incomp :: state.incompatibilities;
                  partial_solution = new_partial_solution;
                  next_id = state.next_id;
                }
              in
              Ok new_state
          | Some previous_satisfier ->
              let previous_satisfier_level =
                get_assignment_level previous_satisfier
              in

              (* Always try to continue resolution to get the full chain *)
              if is_decision satisfier then
                (* Hit a decision - this is as far back as we can trace *)
                let new_assignments =
                  List.filter
                    (fun assignment ->
                      get_assignment_level assignment
                      <= previous_satisfier_level)
                    state.partial_solution.assignments
                in
                let new_partial_solution =
                  {
                    assignments = new_assignments;
                    decision_level = previous_satisfier_level;
                  }
                in
                let new_state =
                  {
                    incompatibilities = incomp :: state.incompatibilities;
                    partial_solution = new_partial_solution;
                    next_id = state.next_id;
                  }
                in
                Ok new_state
              else
                (* Continue resolution to build longer chain *)
                let prior_cause =
                  create_prior_cause incomp satisfier satisfier_term
                    state.next_id
                in
                resolve_conflict prior_cause)
  in
  resolve_conflict conflicting_incomp

and find_earliest_satisfying_assignment partial_solution incomp =
  (* Find the earliest assignment such that incomp is satisfied by partial solution up to and including it *)
  let rec check_assignments prefix_assignments = function
    | [] -> None
    | assignment :: rest ->
        let test_partial =
          {
            partial_solution with
            assignments = prefix_assignments @ [ assignment ];
          }
        in
        if incompatibility_satisfied test_partial incomp then
          (* Find the term in incompatibility that refers to the same package as satisfier *)
          let satisfier_package =
            match assignment with
            | Decision ((name, _), _) -> name
            | Derivation (term, _, _) -> term_package term
          in
          let same_package_term =
            List.find (fun t -> term_package t = satisfier_package) incomp.terms
          in
          Some (assignment, same_package_term)
        else check_assignments (prefix_assignments @ [ assignment ]) rest
  in
  (* Assignments are in reverse chronological order, so reverse to get chronological *)
  let chronological_assignments = List.rev partial_solution.assignments in
  check_assignments [] chronological_assignments

and find_previous_satisfier_for_incomp partial_solution incomp satisfier =
  (* Find earliest assignment before satisfier such that incomp is satisfied by partial solution up to satisfier plus this assignment *)
  let assignments = List.rev partial_solution.assignments in
  (* Convert to chronological order *)
  let rec find_satisfier_index i = function
    | [] -> None
    | a :: _ when a = satisfier -> Some i
    | _ :: rest -> find_satisfier_index (i + 1) rest
  in
  match find_satisfier_index 0 assignments with
  | None -> None
  | Some satisfier_idx ->
      (* Check each assignment before satisfier *)
      let rec check_previous_assignments i =
        if i >= satisfier_idx then None
        else
          let candidate = List.nth assignments i in
          let test_assignments =
            list_take (i + 1) assignments @ [ satisfier ]
          in
          let test_partial =
            { partial_solution with assignments = List.rev test_assignments }
          in
          if incompatibility_satisfied test_partial incomp then Some candidate
          else check_previous_assignments (i + 1)
      in
      check_previous_assignments 0

and is_decision = function Decision _ -> true | Derivation _ -> false

and assignment_satisfies_term assignment term =
  (* Check if an assignment satisfies a specific term *)
  let assignment_packages = get_assignment_packages assignment in
  packages_satisfy_term assignment_packages term

and compute_term_difference satisfier_term target_term =
  (* Compute satisfier_term \ target_term (set difference) *)
  (* This is a simplified version - in a full implementation this would handle version ranges *)
  if term_package satisfier_term = term_package target_term then
    (* Same package - check if versions overlap *)
    let satisfier_versions = term_versions satisfier_term in
    let target_versions = term_versions target_term in
    let difference_versions =
      List.filter (fun v -> not (List.mem v target_versions)) satisfier_versions
    in
    if List.length difference_versions > 0 then
      Some
        (match satisfier_term with
        | Positive (name, _) -> Positive (name, difference_versions)
        | Negative (name, _) -> Negative (name, difference_versions))
    else None
  else
    (* Different packages - no overlap *)
    Some satisfier_term

and create_prior_cause incomp satisfier satisfier_term next_id =
  (* Create prior cause by applying resolution between incomp and satisfier's cause *)
  match satisfier with
  | Decision _ ->
      (* No cause to resolve with - this shouldn't happen in proper conflict resolution *)
      incomp
  | Derivation (satisfier_assignment_term, cause_incomp, _) ->
      (* Union of terms from incomp and cause_incomp, minus terms referring to satisfier's package *)
      let satisfier_package = term_package satisfier_term in
      let incomp_terms =
        List.filter (fun t -> term_package t != satisfier_package) incomp.terms
      in
      let cause_terms =
        List.filter
          (fun t -> term_package t != satisfier_package)
          cause_incomp.terms
      in
      let merged_terms = incomp_terms @ cause_terms in

      (* Check if satisfier doesn't satisfy term - if so, add not (satisfier \ term) *)
      let final_terms =
        if not (assignment_satisfies_term satisfier satisfier_term) then
          (* Add not (satisfier \ term) to priorCause *)
          let satisfier_difference =
            compute_term_difference satisfier_assignment_term satisfier_term
          in
          match satisfier_difference with
          | None -> merged_terms (* No difference to add *)
          | Some diff_term -> negate_term diff_term :: merged_terms
        else merged_terms
      in

      (* Remove duplicates *)
      let unique_terms = List.sort_uniq compare final_terms in
      {
        terms = unique_terms;
        cause = Derived (incomp, cause_incomp);
        id = next_id;
      }

and get_assignment_level = function
  | Decision (_, level) -> level
  | Derivation (_, _, level) -> level

and get_assignment_packages = function
  | Decision (pkg, _) -> [ pkg ]
  | Derivation (Positive (name, versions), _, _) -> (
      match versions with [] -> [] | v :: _ -> [ (name, v) ])
  | Derivation (Negative _, _, _) -> []

(* Error reporting functions *)
let format_external_cause = function
  | Dependency ((dep_name, dep_version), (target_name, _), target_versions) ->
      Printf.sprintf "%s %s depends on %s %s" 
        dep_name dep_version target_name (String.concat " or " target_versions)
  | Unavailable (pkg_name, pkg_version) ->
      Printf.sprintf "%s %s is not available" pkg_name pkg_version
  | Custom msg -> msg

let rec explain_incompatibility incomp depth =
  let indent = String.make (depth * 2) ' ' in
  match incomp.cause with
  | External ext_cause -> Printf.sprintf "%s%s" indent (format_external_cause ext_cause)
  | Derived (cause1, cause2) -> 
      (* Check if this is a simple two-cause derivation for better formatting *)
      match (cause1.cause, cause2.cause) with
      | External ext1, External ext2 ->
          Printf.sprintf "%s%s and %s, but this creates a conflict"
            indent (format_external_cause ext1) (format_external_cause ext2)
      | _ ->
          Printf.sprintf "%s%s\n%sand\n%s" indent
            (explain_incompatibility cause1 (depth + 1))
            indent
            (explain_incompatibility cause2 (depth + 1))

let explain_failure root_incompatibility =
  (* Try to provide more context for common patterns *)
  let basic_explanation = explain_incompatibility root_incompatibility 0 in
  
  (* Check if this looks like a dependency conflict and add context *)
  match root_incompatibility.cause with
  | Derived (cause1, cause2) ->
      (match (cause1.cause, cause2.cause) with
      | (External (Dependency (depender1, _, _)), External (Dependency (depender2, _, _))) ->
          Printf.sprintf "Version solving failed:\n\nBoth %s %s and %s %s are needed, but %s\n\nTherefore, no solution exists."
            (fst depender1) (snd depender1) (fst depender2) (snd depender2) basic_explanation
      | _ ->
          Printf.sprintf "Version solving failed:\n\n%s\n\nTherefore, no solution exists." basic_explanation)
  | _ ->
      Printf.sprintf "Version solving failed:\n\n%s\n\nTherefore, no solution exists." basic_explanation

(* Helper function to check if package has decision *)
let has_decision_for_package package_name partial_solution =
  List.exists
    (function Decision ((name, _), _) -> name = package_name | _ -> false)
    partial_solution.assignments

(* Decision Making Algorithm *)
let make_decision (repo : repository) state =
  (* Find a package that has positive derivations but no decision *)
  let rec find_undecided_package = function
    | [] -> None
    | assignment :: rest -> (
        match assignment with
        | Derivation (Positive (name, versions), _, _) ->
            if has_decision_for_package name state.partial_solution then
              find_undecided_package rest
            else Some (name, versions)
        | _ -> find_undecided_package rest)
  in

  match find_undecided_package state.partial_solution.assignments with
  | None -> None (* No more decisions needed *)
  | Some (package_name, required_versions) -> (
      (* Filter to only versions that are actually available *)
      let available_versions =
        List.filter
          (fun version -> List.mem (package_name, version) repo)
          required_versions
      in

      match available_versions with
      | [] ->
          (* No available versions - this should trigger a conflict *)
          None
      | version :: _ ->
          let new_decision_level = state.partial_solution.decision_level + 1 in
          let new_assignment =
            Decision ((package_name, version), new_decision_level)
          in
          let new_partial_solution =
            add_assignment new_assignment state.partial_solution
          in
          let new_partial_solution =
            { new_partial_solution with decision_level = new_decision_level }
          in
          Some { state with partial_solution = new_partial_solution })

(* Check if we have a complete solution *)
let is_complete_solution state =
  (* Check if all positive derivations have corresponding decisions *)
  let positive_derivations =
    List.filter_map
      (function
        | Derivation (Positive (name, _), _, _) -> Some name | _ -> None)
      state.partial_solution.assignments
  in

  let decisions =
    List.filter_map
      (function Decision ((name, _), _) -> Some name | _ -> None)
      state.partial_solution.assignments
  in

  List.for_all (fun name -> List.mem name decisions) positive_derivations

(* Extract solution from final state *)
let extract_solution state =
  List.filter_map
    (function Decision (pkg, _) -> Some pkg | _ -> None)
    state.partial_solution.assignments

(* Main PubGrub solve function *)
let solve (repo : repository) (deps : dependencies) (query : query) :
    solve_result =
  let initial_state = create_initial_state repo deps query in

  let rec solve_loop state next_package : solve_result =
    (* Unit propagation *)
    match unit_propagation repo state [ next_package ] with
    | Error err -> Error err
    | Ok new_state -> (
        if is_complete_solution new_state then
          Solution (extract_solution new_state)
        else
          (* Decision making *)
          match make_decision repo new_state with
          | None ->
              Solution (extract_solution new_state)
              (* No more decisions needed *)
          | Some newer_state -> (
              (* Get the package name from the most recent decision *)
              match newer_state.partial_solution.assignments with
              | Decision ((name, _), _) :: _ -> solve_loop newer_state name
              | _ -> Error (InvalidInput "Invalid state after decision")))
  in

  (* Start with root package from query *)
  match query with
  | [] -> Solution []
  | (name, _) :: _ -> solve_loop initial_state name
