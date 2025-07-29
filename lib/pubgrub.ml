let debug_enabled = ref false

let debug_printf fmt =
  if !debug_enabled then Printf.printf fmt else Printf.ifprintf stdout fmt

let set_debug enabled = debug_enabled := enabled

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
  | Dependency of
      package
      * package
      * string list (* depender depends on target with versions *)
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
type solve_error = NoSolution of incompatibility | InvalidInput of string
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
      | Derivation (Positive (name, [ version ]), _, _) ->
          (* Only count single-version positive derivations as assignments *)
          (name, version) :: acc
      | Derivation (Positive (_, _), _, _) ->
          (* Multi-version derivations don't select a specific version *)
          acc
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
  debug_printf "DEBUG: unit_propagation called with packages: [%s]\n"
    (String.concat "; " package_names);
  if !debug_enabled then flush_all ();
  match package_names with
  | [] ->
      debug_printf "DEBUG: unit_propagation done (empty package list)\n";
      if !debug_enabled then flush_all ();
      Ok state
  | package_name :: rest -> (
      debug_printf "DEBUG: processing package %s\n" package_name;
      if !debug_enabled then flush_all ();
      let relevant_incomps =
        List.filter
          (fun incomp ->
            List.mem package_name (incompatibility_packages incomp))
          state.incompatibilities
      in

      match process_incompatibilities repo state relevant_incomps with
      | Ok (new_state, changed_pkg_opt) ->
          (* Use changed package from conflict resolution or continue with rest *)
          let next_packages =
            match changed_pkg_opt with
            | Some pkg ->
                [ pkg ] (* Conflict resolution - replace changed set *)
            | None -> rest (* Normal propagation - continue with rest *)
          in
          unit_propagation repo new_state next_packages
      | Error err -> Error err)

and process_incompatibilities (repo : repository) state incomps :
    (solver_state * string option) internal_result =
  match incomps with
  | [] ->
      debug_printf "DEBUG: process_incompatibilities done (no incomps)\n";
      if !debug_enabled then flush_all ();
      Ok (state, None)
  | incomp :: rest ->
      debug_printf "DEBUG: checking incomp ID %d\n" incomp.id;
      if !debug_enabled then flush_all ();
      if incompatibility_satisfied state.partial_solution incomp then (
        (* Conflict detected - need conflict resolution *)
        debug_printf "DEBUG: incomp %d is satisfied - CONFLICT!\n" incomp.id;
        if !debug_enabled then flush_all ();
        match conflict_resolution state incomp with
        | Ok (new_state, learned_incomp) ->
            debug_printf "DEBUG: conflict resolution succeeded\n";
            if !debug_enabled then flush_all ();
            (* After backtracking, continue processing with the learned incompatibility *)
            let new_state_with_learned =
              {
                new_state with
                incompatibilities =
                  (if
                     List.exists
                       (fun i -> i.id = learned_incomp.id)
                       new_state.incompatibilities
                   then new_state.incompatibilities
                   else learned_incomp :: new_state.incompatibilities);
              }
            in
            if
              incompatibility_satisfied new_state_with_learned.partial_solution
                learned_incomp
            then
              if
                (* Check if this is a root-level conflict *)
                List.length learned_incomp.terms = 0
                || List.length learned_incomp.terms = 1
                   &&
                   match learned_incomp.terms with
                   | [ Positive (name, versions) ] ->
                       (* Check if this is a query package *)
                       List.exists
                         (fun assignment ->
                           match assignment with
                           | Decision ((n, v), 0)
                             when n = name && List.mem v versions ->
                               true
                           | _ -> false)
                         new_state_with_learned.partial_solution.assignments
                   | _ -> false
              then (
                debug_printf "DEBUG: Root-level conflict - no solution!\n";
                if !debug_enabled then flush_all ();
                Error (NoSolution learned_incomp))
              else (
                (* Continue with conflict resolution *)
                debug_printf
                  "DEBUG: Learned incomp still satisfied after backtrack, \
                   continuing resolution\n";
                if !debug_enabled then flush_all ();
                process_incompatibilities repo new_state_with_learned
                  (learned_incomp :: incomps))
            else if
              incompatibility_almost_satisfied
                new_state_with_learned.partial_solution learned_incomp
            then (
              (* Find the package name from the learned incompatibility to focus on *)
              let unsatisfied_term =
                get_unsatisfied_term new_state_with_learned.partial_solution
                  learned_incomp
              in
              let changed_pkg = term_package unsatisfied_term in
              debug_printf
                "DEBUG: focusing on package %s after conflict resolution\n"
                changed_pkg;
              if !debug_enabled then flush_all ();
              Ok (new_state_with_learned, Some changed_pkg))
            else (
              (* The learned incompatibility is not immediately applicable, continue *)
              debug_printf
                "DEBUG: Learned incomp not immediately applicable, continuing\n";
              if !debug_enabled then flush_all ();
              process_incompatibilities repo new_state_with_learned rest)
        | Error err ->
            debug_printf "DEBUG: conflict resolution failed\n";
            if !debug_enabled then flush_all ();
            Error err)
      else if incompatibility_almost_satisfied state.partial_solution incomp
      then (
        debug_printf "DEBUG: incomp %d is almost satisfied - unit propagation\n"
          incomp.id;
        if !debug_enabled then flush_all ();
        (* Unit propagation possible *)
        let unsatisfied_term =
          get_unsatisfied_term state.partial_solution incomp
        in
        let negated_term = negate_term unsatisfied_term in

        (* Check if positive derivation requires unavailable packages *)
        match negated_term with
        | Positive (name, versions) -> (
            let available_versions =
              List.filter
                (fun version -> List.mem (name, version) repo)
                versions
            in
            if List.length available_versions = 0 then
              (* Create incompatibilities showing the full chain: dependency + unavailability *)
              let unavailable_incomp =
                {
                  terms = [ Positive (name, versions) ];
                  cause =
                    External (Unavailable (name, String.concat " or " versions));
                  id = state.next_id;
                }
              in
              let derived_incomp =
                {
                  terms =
                    List.filter (fun t -> term_package t <> name) incomp.terms;
                  cause = Derived (incomp, unavailable_incomp);
                  id = state.next_id + 1;
                }
              in
              Error (NoSolution derived_incomp)
            else
              (* One or more versions available - check for conflicts before deriving *)
              (* Check if this positive derivation conflicts with existing negative derivations *)
              let conflicting_negative_derivation =
                let rec find_conflict = function
                  | [] -> None
                  | Derivation (Negative (n, neg_versions), cause_incomp, _)
                    :: _
                    when n = name ->
                      (* Check if all available versions are forbidden *)
                      if
                        List.for_all
                          (fun v -> List.mem v neg_versions)
                          available_versions
                      then Some cause_incomp
                      else None
                  | _ :: rest -> find_conflict rest
                in
                find_conflict state.partial_solution.assignments
              in
              match conflicting_negative_derivation with
              | Some neg_cause_incomp ->
                  (* We have a conflict - derive a new incompatibility *)
                  debug_printf
                    "DEBUG: Positive derivation %s conflicts with negative \
                     derivation\n"
                    name;
                  debug_printf "  Negative was caused by incomp ID %d\n"
                    neg_cause_incomp.id;
                  if !debug_enabled then flush_all ();
                  (* Create a derived incompatibility: the positive requirement is incompatible
                   with whatever caused the negative derivation *)
                  let derived_incomp =
                    {
                      terms =
                        List.filter
                          (fun t -> term_package t <> name)
                          incomp.terms;
                      cause = Derived (incomp, neg_cause_incomp);
                      id = state.next_id;
                    }
                  in
                  let new_state =
                    {
                      state with
                      incompatibilities =
                        derived_incomp :: state.incompatibilities;
                      next_id = state.next_id + 1;
                    }
                  in
                  debug_printf "DEBUG: Created derived incomp ID %d\n"
                    derived_incomp.id;
                  (* Check if this derived incompatibility is satisfied *)
                  if
                    incompatibility_satisfied new_state.partial_solution
                      derived_incomp
                  then (
                    debug_printf
                      "DEBUG: Derived incomp is satisfied, entering conflict \
                       resolution\n";
                    if !debug_enabled then flush_all ();
                    match conflict_resolution new_state derived_incomp with
                    | Ok (new_state, learned_incomp) ->
                        debug_printf "DEBUG: conflict resolution succeeded\n";
                        if !debug_enabled then flush_all ();
                        (* After backtracking, continue processing with the learned incompatibility *)
                        let new_state_with_learned =
                          {
                            new_state with
                            incompatibilities =
                              (if
                                 List.exists
                                   (fun i -> i.id = learned_incomp.id)
                                   new_state.incompatibilities
                               then new_state.incompatibilities
                               else
                                 learned_incomp :: new_state.incompatibilities);
                          }
                        in
                        if
                          incompatibility_satisfied
                            new_state_with_learned.partial_solution
                            learned_incomp
                        then
                          if
                            (* Check if this is a root-level conflict *)
                            List.length learned_incomp.terms = 0
                            || List.length learned_incomp.terms = 1
                               &&
                               match learned_incomp.terms with
                               | [ Positive (name, versions) ] ->
                                   (* Check if this is a query package *)
                                   List.exists
                                     (fun assignment ->
                                       match assignment with
                                       | Decision ((n, v), 0)
                                         when n = name && List.mem v versions ->
                                           true
                                       | _ -> false)
                                     new_state_with_learned.partial_solution
                                       .assignments
                               | _ -> false
                          then (
                            debug_printf
                              "DEBUG: Root-level conflict - no solution!\n";
                            if !debug_enabled then flush_all ();
                            Error (NoSolution learned_incomp))
                          else (
                            (* Continue with conflict resolution *)
                            debug_printf
                              "DEBUG: Learned incomp still satisfied after \
                               backtrack, continuing resolution\n";
                            if !debug_enabled then flush_all ();
                            process_incompatibilities repo
                              new_state_with_learned
                              (learned_incomp :: incomps))
                        else if
                          incompatibility_almost_satisfied
                            new_state_with_learned.partial_solution
                            learned_incomp
                        then (
                          (* Find the package name from the learned incompatibility to focus on *)
                          let unsatisfied_term =
                            get_unsatisfied_term
                              new_state_with_learned.partial_solution
                              learned_incomp
                          in
                          let changed_pkg = term_package unsatisfied_term in
                          debug_printf
                            "DEBUG: focusing on package %s after conflict \
                             resolution\n"
                            changed_pkg;
                          if !debug_enabled then flush_all ();
                          Ok (new_state_with_learned, Some changed_pkg))
                        else (
                          (* The learned incompatibility is not immediately applicable, continue *)
                          debug_printf
                            "DEBUG: Learned incomp not immediately applicable, \
                             continuing\n";
                          if !debug_enabled then flush_all ();
                          process_incompatibilities repo new_state_with_learned
                            rest)
                    | Error err ->
                        debug_printf "DEBUG: conflict resolution failed\n";
                        if !debug_enabled then flush_all ();
                        Error err)
                  else
                    (* Derived incomp not satisfied, continue processing *)
                    process_incompatibilities repo new_state
                      (derived_incomp :: rest)
              | _ -> (
                  (* No conflict, proceed normally *)
                  let new_assignment =
                    Derivation
                      ( negated_term,
                        incomp,
                        state.partial_solution.decision_level )
                  in
                  let new_partial_solution =
                    add_assignment new_assignment state.partial_solution
                  in
                  let new_state =
                    { state with partial_solution = new_partial_solution }
                  in
                  debug_printf "DEBUG: added positive derivation, continuing\n";
                  if !debug_enabled then flush_all ();
                  match process_incompatibilities repo new_state rest with
                  | Ok (final_state, changed_pkg) ->
                      Ok (final_state, changed_pkg)
                  | Error err -> Error err))
        | Negative _ -> (
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
            debug_printf "DEBUG: added negative derivation: %s %s, continuing\n"
              (term_package negated_term)
              (if is_positive negated_term then "MUST" else "MUST NOT");
            if !debug_enabled then flush_all ();
            match process_incompatibilities repo new_state rest with
            | Ok (final_state, changed_pkg) -> Ok (final_state, changed_pkg)
            | Error err -> Error err))
      else (
        debug_printf
          "DEBUG: incomp %d not satisfied or almost satisfied, skipping\n"
          incomp.id;
        if !debug_enabled then flush_all ();
        match process_incompatibilities repo state rest with
        | Ok (final_state, changed_pkg) -> Ok (final_state, changed_pkg)
        | Error err -> Error err)

(* Conflict Resolution Algorithm *)
and conflict_resolution state conflicting_incomp :
    (solver_state * incompatibility) internal_result =
  debug_printf "DEBUG: Conflict resolution for incomp ID %d\n"
    conflicting_incomp.id;
  debug_printf "  Terms in conflicting incomp:\n";
  List.iter
    (fun t ->
      debug_printf "    %s %s [%s]\n" (term_package t)
        (if is_positive t then "+" else "-")
        (String.concat "," (term_versions t)))
    conflicting_incomp.terms;
  if !debug_enabled then flush_all ();

  let rec resolve_conflict incomp iterations =
    debug_printf "DEBUG: Resolving conflict iteration %d for incomp ID %d\n"
      iterations incomp.id;

    (* Check if this is a root-level failure: empty terms or contains query packages *)
    if List.length incomp.terms = 0 then (
      debug_printf "DEBUG: Root-level failure - no solution\n";
      Error (NoSolution incomp))
    else
      (* Find the earliest assignment that makes the incompatibility satisfied *)
      match
        find_earliest_satisfying_assignment state.partial_solution incomp
      with
      | None ->
          debug_printf "DEBUG: No satisfying assignment found\n";
          (* This might be due to a conflict between positive requirements and negative derivations *)
          (* For now, we'll treat this as a root cause that needs to be traced back further *)
          (* Create a derived incompatibility from the conflicting terms *)
          let conflicting_terms =
            List.filter
              (fun term ->
                match term with
                | Positive (name, _) ->
                    (* Check if we have assignments that would satisfy this positive term *)
                    List.exists
                      (function
                        | Decision ((n, _), _) when n = name -> true
                        | Derivation (Positive (n, _), _, _) when n = name ->
                            true
                        | _ -> false)
                      state.partial_solution.assignments
                | _ -> true)
              incomp.terms
          in
          if List.length conflicting_terms > 0 then (
            debug_printf
              "DEBUG: Creating derived incompatibility from conflicting terms\n";
            let derived_incomp =
              {
                terms = conflicting_terms;
                cause = incomp.cause;
                (* Keep the original cause for now *)
                id = state.next_id;
              }
            in
            Error (NoSolution derived_incomp))
          else Error (NoSolution incomp)
      | Some (satisfier, satisfier_term) -> (
          debug_printf "DEBUG: Found satisfier for term %s\n"
            (term_package satisfier_term);
          debug_printf "  Satisfier type: %s\n"
            (match satisfier with
            | Decision _ -> "Decision"
            | Derivation _ -> "Derivation");
          debug_printf "  Satisfier level: %d\n"
            (get_assignment_level satisfier);
          (* Find previous satisfier *)
          match
            find_previous_satisfier_for_incomp state.partial_solution incomp
              satisfier
          with
          | None ->
              debug_printf
                "DEBUG: No previous satisfier - backtracking to level 1\n";
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
                  next_id = state.next_id + 1;
                }
              in
              debug_printf "DEBUG: Added learned incomp ID %d to state\n"
                incomp.id;
              debug_printf "  State now has %d incompatibilities\n"
                (List.length new_state.incompatibilities);
              Ok (new_state, incomp)
          | Some previous_satisfier ->
              let previous_satisfier_level =
                get_assignment_level previous_satisfier
              in
              debug_printf "DEBUG: Found previous satisfier at level %d\n"
                previous_satisfier_level;
              debug_printf "  Previous satisfier type: %s\n"
                (match previous_satisfier with
                | Decision _ -> "Decision"
                | Derivation _ -> "Derivation");

              (* Always derive the new incompatibility first *)
              let derived_incomp =
                if is_decision satisfier then
                  (* If satisfier is a decision, the conflicting incompatibility is the result *)
                  incomp
                else
                  match satisfier with
                  | Decision _ -> incomp (* shouldn't happen, but handle it *)
                  | Derivation (_, cause_incomp, _) ->
                      (* Apply resolution rule: derive new incompatibility from incomp and cause_incomp *)
                      let satisfier_package = term_package satisfier_term in
                      let incomp_terms =
                        List.filter
                          (fun t -> term_package t <> satisfier_package)
                          incomp.terms
                      in
                      let cause_terms =
                        List.filter
                          (fun t -> term_package t <> satisfier_package)
                          cause_incomp.terms
                      in
                      let merged_terms = incomp_terms @ cause_terms in

                      (* Remove duplicates *)
                      let unique_terms = List.sort_uniq compare merged_terms in
                      debug_printf "DEBUG: Deriving incomp from %d and %d\n"
                        incomp.id cause_incomp.id;
                      debug_printf "  Merged terms:\n";
                      List.iter
                        (fun t ->
                          debug_printf "    %s %s [%s]\n" (term_package t)
                            (if is_positive t then "+" else "-")
                            (String.concat "," (term_versions t)))
                        unique_terms;
                      {
                        terms = unique_terms;
                        cause = Derived (incomp, cause_incomp);
                        id = state.next_id + iterations;
                      }
              in

              (* Check if we should backtrack or continue resolution *)
              if
                is_decision satisfier
                || get_assignment_level satisfier <> previous_satisfier_level
              then (
                debug_printf
                  "DEBUG: Satisfier is decision or different level - \
                   backtracking\n";
                debug_printf "  is_decision satisfier: %b\n"
                  (is_decision satisfier);
                debug_printf "  satisfier level: %d, previous level: %d\n"
                  (get_assignment_level satisfier)
                  previous_satisfier_level;
                debug_printf "  Derived incomp ID %d with %d terms\n"
                  derived_incomp.id
                  (List.length derived_incomp.terms);
                (* Backtrack to previous satisfier level *)
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
                    incompatibilities =
                      derived_incomp :: state.incompatibilities;
                    partial_solution = new_partial_solution;
                    next_id = state.next_id + iterations + 1;
                  }
                in
                debug_printf "DEBUG: Added learned incomp ID %d to state\n"
                  derived_incomp.id;
                debug_printf "  State now has %d incompatibilities\n"
                  (List.length new_state.incompatibilities);
                Ok (new_state, derived_incomp))
              else (
                (* Continue resolution with the derived incompatibility *)
                debug_printf
                  "DEBUG: Continuing resolution - recursing with derived incomp\n";
                debug_printf "DEBUG: Created prior cause ID %d with %d terms\n"
                  derived_incomp.id
                  (List.length derived_incomp.terms);

                if iterations > 50 then (
                  debug_printf
                    "DEBUG: Hit iteration limit in conflict resolution\n";
                  Error (NoSolution derived_incomp))
                else resolve_conflict derived_incomp (iterations + 1)))
  in
  resolve_conflict conflicting_incomp 0

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
  debug_printf "DEBUG: create_prior_cause called:\n";
  debug_printf "  incomp ID %d with %d terms\n" incomp.id
    (List.length incomp.terms);
  debug_printf "  satisfier_term: %s %s\n"
    (term_package satisfier_term)
    (if is_positive satisfier_term then "+" else "-");
  if !debug_enabled then flush_all ();

  (* Create prior cause by applying resolution between incomp and satisfier's cause *)
  match satisfier with
  | Decision _ ->
      debug_printf "  satisfier is Decision, returning original incomp\n";
      if !debug_enabled then flush_all ();
      (* No cause to resolve with - this shouldn't happen in proper conflict resolution *)
      incomp
  | Derivation (satisfier_assignment_term, cause_incomp, _) ->
      debug_printf "  satisfier is Derivation from incomp ID %d\n"
        cause_incomp.id;
      if !debug_enabled then flush_all ();
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
      let result =
        {
          terms = unique_terms;
          cause = Derived (incomp, cause_incomp);
          id = next_id;
        }
      in
      debug_printf "  created prior_cause ID %d with %d terms\n" result.id
        (List.length result.terms);
      List.iter
        (fun t ->
          debug_printf "    %s %s\n" (term_package t)
            (if is_positive t then "+" else "-"))
        result.terms;
      if !debug_enabled then flush_all ();
      result

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
      Printf.sprintf "%s %s depends on %s %s" dep_name dep_version target_name
        (String.concat " or " target_versions)
  | Unavailable (pkg_name, pkg_version) ->
      Printf.sprintf "%s %s is not available" pkg_name pkg_version
  | Custom msg -> msg

let rec explain_incompatibility_tree incomp =
  match incomp.cause with
  | External ext_cause -> `Leaf (format_external_cause ext_cause)
  | Derived (cause1, cause2) ->
      `Node
        ( explain_incompatibility_tree cause1,
          explain_incompatibility_tree cause2 )

let rec format_tree = function
  | `Leaf s -> s
  | `Node (t1, t2) ->
      let s1 = format_tree t1 in
      let s2 = format_tree t2 in
      Printf.sprintf "(%s and %s)" s1 s2

let explain_incompatibility root_incomp =
  let tree = explain_incompatibility_tree root_incomp in
  format_tree tree

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

      (* Filter out versions that would conflict with negative derivations *)
      let compatible_versions =
        List.filter
          (fun version ->
            (* Check if this version conflicts with any negative derivations *)
            not
              (List.exists
                 (function
                   | Derivation (Negative (n, versions), _, _)
                     when n = package_name ->
                       List.mem version versions
                   | _ -> false)
                 state.partial_solution.assignments))
          available_versions
      in

      match compatible_versions with
      | [] ->
          (* No compatible versions - this should trigger a conflict *)
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

  (* All positive derivations need decisions to trigger dependency loading *)
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
  let solve_iterations = ref 0 in

  let rec solve_loop state next_package : solve_result =
    incr solve_iterations;
    debug_printf "DEBUG: Solve iteration %d, next_package: %s\n"
      !solve_iterations next_package;
    if !solve_iterations > 200 then (
      debug_printf "DEBUG: Solve hit iteration limit\n";
      Error (InvalidInput "Solve iteration limit reached"))
    else
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
  debug_printf "DEBUG: Starting solve with query length %d\n"
    (List.length query);
  if !debug_enabled then flush_all ();
  match query with
  | [] ->
      debug_printf "DEBUG: Empty query, returning empty solution\n";
      if !debug_enabled then flush_all ();
      Solution []
  | (name, _) :: _ ->
      debug_printf "DEBUG: Starting solve_loop with package %s\n" name;
      if !debug_enabled then flush_all ();
      solve_loop initial_state name
