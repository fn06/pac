open Pac.Pubgrub

(* Simple step-by-step manual debugging *)
let manual_debug () =
  Printf.printf "=== MANUAL DEBUG SESSION ===\n\n";
  
  (* Test case: A depends on B, and B exists with no dependencies *)
  let deps = [
    (("A", "1"), ("B", ["1"]));
    (* B 1 has no dependencies - not represented in dependency list *)
  ] in
  let query = [("A", "1")] in
  
  Printf.printf "1. Creating initial state...\n";
  let initial_state = create_initial_state deps query in
  
  Printf.printf "   Incompatibilities: %d\n" (List.length initial_state.incompatibilities);
  Printf.printf "   Assignments: %d\n" (List.length initial_state.partial_solution.assignments);
  Printf.printf "   Decision level: %d\n" initial_state.partial_solution.decision_level;
  
  Printf.printf "\n2. Examining initial assignments:\n";
  List.iteri (fun i assignment ->
    match assignment with
    | Decision ((name, version), level) ->
        Printf.printf "   %d. Decision: %s %s (level %d)\n" i name version level
    | Derivation (term, _, level) ->
        Printf.printf "   %d. Derivation: %s %s (level %d)\n" i 
          (term_package term)
          (if is_positive term then "POS" else "NEG")
          level
  ) initial_state.partial_solution.assignments;
  
  Printf.printf "\n3. Examining incompatibilities:\n";
  List.iteri (fun i incomp ->
    Printf.printf "   %d. ID:%d Terms:%d\n" i incomp.id (List.length incomp.terms);
    List.iteri (fun j term ->
      Printf.printf "      %d.%d: %s %s [%s]\n" i j
        (term_package term)
        (if is_positive term then "POS" else "NEG")
        (String.concat "," (term_versions term))
    ) incomp.terms
  ) initial_state.incompatibilities;
  
  Printf.printf "\n4. Testing unit propagation manually...\n";
  let package_to_process = "A" in
  Printf.printf "   Processing package: %s\n" package_to_process;
  
  let relevant_incomps = List.filter (fun incomp ->
    List.mem package_to_process (incompatibility_packages incomp)
  ) initial_state.incompatibilities in
  
  Printf.printf "   Found %d relevant incompatibilities\n" (List.length relevant_incomps);
  
  Printf.printf "\n5. Checking each relevant incompatibility:\n";
  List.iteri (fun i incomp ->
    Printf.printf "   Incompatibility %d (ID:%d):\n" i incomp.id;
    
    let satisfied = incompatibility_satisfied initial_state.partial_solution incomp in
    let almost_satisfied = incompatibility_almost_satisfied initial_state.partial_solution incomp in
    
    Printf.printf "     Satisfied: %b\n" satisfied;
    Printf.printf "     Almost satisfied: %b\n" almost_satisfied;
    
    if satisfied then
      Printf.printf "     *** CONFLICT DETECTED ***\n"
    else if almost_satisfied then (
      Printf.printf "     *** UNIT PROPAGATION POSSIBLE ***\n";
      let unsatisfied_term = get_unsatisfied_term initial_state.partial_solution incomp in
      Printf.printf "     Unsatisfied term: %s %s\n" 
        (term_package unsatisfied_term)
        (if is_positive unsatisfied_term then "POS" else "NEG");
      let negated = negate_term unsatisfied_term in
      Printf.printf "     Would add derivation: %s %s\n"
        (term_package negated)
        (if is_positive negated then "POS" else "NEG")
    ) else (
      Printf.printf "     Neither satisfied nor almost satisfied\n";
      Printf.printf "     Checking individual terms:\n";
      List.iteri (fun j term ->
        let satisfied_term = term_satisfied_by_partial_solution term initial_state.partial_solution in
        let contradicted_term = term_contradicted_by_partial_solution term initial_state.partial_solution in
        Printf.printf "       Term %d: %s %s - satisfied:%b contradicted:%b\n" j
          (term_package term)
          (if is_positive term then "POS" else "NEG")
          satisfied_term
          contradicted_term
      ) incomp.terms
    )
  ) relevant_incomps;
  
  Printf.printf "\n6. Testing is_complete_solution...\n";
  let complete = is_complete_solution initial_state in
  Printf.printf "   Is complete: %b\n" complete;
  
  if not complete then (
    Printf.printf "\n7. Testing make_decision...\n";
    match make_decision initial_state with
    | None -> Printf.printf "   No decision to make\n"
    | Some new_state ->
        Printf.printf "   Decision made!\n";
        Printf.printf "   New assignments: %d\n" (List.length new_state.partial_solution.assignments);
        Printf.printf "   New decision level: %d\n" new_state.partial_solution.decision_level;
        
        Printf.printf "   New assignments list:\n";
        List.iteri (fun i assignment ->
          match assignment with
          | Decision ((name, version), level) ->
              Printf.printf "     %d. Decision: %s %s (level %d)\n" i name version level
          | Derivation (term, _, level) ->
              Printf.printf "     %d. Derivation: %s %s (level %d)\n" i 
                (term_package term)
                (if is_positive term then "POS" else "NEG")
                level
        ) new_state.partial_solution.assignments
  );
  
  Printf.printf "\n=== MANUAL DEBUG COMPLETE ===\n"

let () = manual_debug ()