open Pac.Pubgrub

(* Debug counter to prevent infinite loops *)
let debug_counter = ref 0
let max_iterations = 20

(* Debug version of solve with step-by-step tracing *)
let rec debug_solve (deps : dependencies) (query : query) : solve_result =
  Printf.printf "\n=== DEBUG SOLVE START ===\n";
  Printf.printf "Dependencies: %d\n" (List.length deps);
  Printf.printf "Query: %d packages\n" (List.length query);
  
  let initial_state = create_initial_state deps query in
  Printf.printf "Initial state created:\n";
  Printf.printf "  Incompatibilities: %d\n" (List.length initial_state.incompatibilities);
  Printf.printf "  Initial assignments: %d\n" (List.length initial_state.partial_solution.assignments);
  
  let rec debug_solve_loop state next_package iteration =
    Printf.printf "\n--- ITERATION %d ---\n" iteration;
    Printf.printf "Processing package: %s\n" next_package;
    Printf.printf "Current assignments: %d\n" (List.length state.partial_solution.assignments);
    Printf.printf "Decision level: %d\n" state.partial_solution.decision_level;
    
    if iteration > max_iterations then (
      Printf.printf "STOPPING: Hit max iterations (%d)\n" max_iterations;
      Error (InvalidInput "Debug: max iterations reached")
    ) else (
      Printf.printf "Starting unit propagation...\n";
      
      (* Debug unit propagation *)
      match debug_unit_propagation state [next_package] (iteration * 10) with
      | Error err -> 
          Printf.printf "Unit propagation failed: %s\n" 
            (match err with 
             | NoSolution _ -> "NoSolution" 
             | InvalidInput msg -> "InvalidInput: " ^ msg);
          Error err
      | Ok new_state ->
          Printf.printf "Unit propagation completed\n";
          Printf.printf "New assignments: %d\n" (List.length new_state.partial_solution.assignments);
          
          if debug_is_complete_solution new_state then (
            Printf.printf "COMPLETE SOLUTION FOUND!\n";
            Solution (extract_solution new_state)
          ) else (
            Printf.printf "Solution incomplete, making decision...\n";
            match debug_make_decision new_state with
            | None -> 
                Printf.printf "No more decisions needed\n";
                Solution (extract_solution new_state)
            | Some newer_state ->
                Printf.printf "Decision made, continuing...\n";
                match newer_state.partial_solution.assignments with
                | Decision ((name, _), _) :: _ -> 
                    debug_solve_loop newer_state name (iteration + 1)
                | _ -> 
                    Printf.printf "ERROR: Invalid state after decision\n";
                    Error (InvalidInput "Invalid state after decision")
          )
    )
  in
  
  match query with
  | [] -> Solution []
  | (name, _) :: _ -> debug_solve_loop initial_state name 1

(* Debug unit propagation with iteration tracking *)
and debug_unit_propagation state package_names iteration : solver_state internal_result =
  Printf.printf "    Unit propagation iteration %d\n" iteration;
  Printf.printf "    Processing packages: [%s]\n" (String.concat "; " package_names);
  
  if iteration > max_iterations + 10 then (
    Printf.printf "    STOPPING: Unit propagation hit max iterations\n";
    Error (InvalidInput "Debug: unit propagation max iterations")
  ) else (
    match package_names with
    | [] -> 
        Printf.printf "    Unit propagation complete (no more packages)\n";
        Ok state
    | package_name :: rest ->
        Printf.printf "    Checking incompatibilities for package: %s\n" package_name;
        let relevant_incomps = List.filter (fun incomp ->
          List.mem package_name (incompatibility_packages incomp)
        ) state.incompatibilities in
        Printf.printf "    Found %d relevant incompatibilities\n" (List.length relevant_incomps);
        
        match debug_process_incompatibilities state relevant_incomps (iteration * 10) with
        | Ok new_state ->
            let changed_packages = if new_state != state then [package_name] else [] in
            Printf.printf "    Continuing with packages: [%s]\n" 
              (String.concat "; " (rest @ changed_packages));
            debug_unit_propagation new_state (rest @ changed_packages) (iteration + 1)
        | Error err -> Error err
  )

(* Debug process incompatibilities *)
and debug_process_incompatibilities state incomps iteration : solver_state internal_result =
  Printf.printf "      Processing %d incompatibilities (iter %d)\n" (List.length incomps) iteration;
  
  if iteration > max_iterations + 20 then (
    Printf.printf "      STOPPING: Process incompatibilities hit max iterations\n";
    Error (InvalidInput "Debug: process incompatibilities max iterations")
  ) else (
    match incomps with
    | [] -> 
        Printf.printf "      All incompatibilities processed\n";
        Ok state
    | incomp :: rest ->
        Printf.printf "      Checking incompatibility ID: %d\n" incomp.id;
        
        if incompatibility_satisfied state.partial_solution incomp then (
          Printf.printf "      CONFLICT detected in incompatibility %d\n" incomp.id;
          Printf.printf "      Starting conflict resolution...\n";
          match conflict_resolution state incomp with
          | Ok new_state -> 
              Printf.printf "      Conflict resolved\n";
              Ok new_state
          | Error err -> 
              Printf.printf "      Conflict resolution failed\n";
              Error err
        ) else if incompatibility_almost_satisfied state.partial_solution incomp then (
          Printf.printf "      Unit propagation possible for incompatibility %d\n" incomp.id;
          let unsatisfied_term = get_unsatisfied_term state.partial_solution incomp in
          let negated_term = negate_term unsatisfied_term in
          Printf.printf "      Adding derivation: %s %s\n" 
            (term_package negated_term)
            (if is_positive negated_term then "POS" else "NEG");
          
          let new_assignment = Derivation (negated_term, incomp, state.partial_solution.decision_level) in
          let new_partial_solution = add_assignment new_assignment state.partial_solution in
          let new_state = { state with partial_solution = new_partial_solution } in
          debug_process_incompatibilities new_state rest (iteration + 1)
        ) else (
          Printf.printf "      Incompatibility %d not applicable\n" incomp.id;
          debug_process_incompatibilities state rest iteration
        )
  )

(* Debug versions of other functions *)
and debug_is_complete_solution state =
  let positive_derivations = List.filter_map (function
    | Derivation (Positive (name, _), _, _) -> Some name | _ -> None
  ) state.partial_solution.assignments in
  
  let decisions = List.filter_map (function
    | Decision ((name, _), _) -> Some name | _ -> None
  ) state.partial_solution.assignments in
  
  Printf.printf "    Checking completeness: %d positive derivations, %d decisions\n"
    (List.length positive_derivations) (List.length decisions);
  
  List.for_all (fun name -> List.mem name decisions) positive_derivations

and debug_make_decision state =
  Printf.printf "    Looking for undecided packages...\n";
  
  let rec find_undecided_package = function
    | [] -> 
        Printf.printf "    No undecided packages found\n";
        None
    | assignment :: rest ->
        match assignment with
        | Derivation (Positive (name, versions), _, _) ->
            Printf.printf "    Found positive derivation for %s\n" name;
            if has_decision_for_package name state.partial_solution then (
              Printf.printf "    %s already has decision, continuing...\n" name;
              find_undecided_package rest
            ) else (
              Printf.printf "    %s needs decision from versions: [%s]\n" name 
                (String.concat "," versions);
              Some (name, versions)
            )
        | _ -> find_undecided_package rest
  in
  
  match find_undecided_package state.partial_solution.assignments with
  | None -> None
  | Some (package_name, available_versions) ->
      match available_versions with
      | [] -> 
          Printf.printf "    No available versions for %s\n" package_name;
          None
      | version :: _ ->
          Printf.printf "    Making decision: %s %s\n" package_name version;
          let new_decision_level = state.partial_solution.decision_level + 1 in
          let new_assignment = Decision ((package_name, version), new_decision_level) in
          let new_partial_solution = add_assignment new_assignment state.partial_solution in
          let new_partial_solution = { new_partial_solution with decision_level = new_decision_level } in
          Some { state with partial_solution = new_partial_solution }

(* Test the debug solver *)
let test_debug_simple () =
  Printf.printf "\n=== DEBUG TEST: Simple Dependency ===\n";
  
  let deps = [
    (("A", "1"), ("B", ["1"]));
  ] in
  let query = [("A", "1")] in
  
  match debug_solve deps query with
  | Solution packages -> 
      Printf.printf "\nDEBUG RESULT: Solution found!\n";
      List.iter (fun (name, version) -> 
        Printf.printf "  %s %s\n" name version
      ) packages
  | Error (NoSolution incomp) ->
      Printf.printf "\nDEBUG RESULT: No solution (incompatibility %d)\n" incomp.id
  | Error (InvalidInput msg) ->
      Printf.printf "\nDEBUG RESULT: %s\n" msg

let () = test_debug_simple ()