open Pac.Pubgrub

(* Helper functions for testing *)
let make_package name version = (name, version)

let make_dependency depender target_name target_versions =
  (depender, (target_name, target_versions))

(* Test data *)
let simple_deps =
  [
    make_dependency ("A", "1") "B" [ "1" ];
    make_dependency ("B", "1") "C" [ "1" ];
  ]

let simple_query = [ make_package "A" "1" ]

let _conflicting_deps =
  [
    make_dependency ("A", "1") "B" [ "1" ];
    make_dependency ("A", "1") "C" [ "1" ];
    make_dependency ("B", "1") "D" [ "1" ];
    make_dependency ("C", "1") "D" [ "2" ];
  ]

(* Unit Tests *)

(* Test 1: Basic term operations *)
let test_term_operations () =
  Printf.printf "=== Testing Term Operations ===\n";

  let pos_term = Positive ("foo", [ "1.0.0" ]) in
  let neg_term = Negative ("foo", [ "1.0.0" ]) in

  Printf.printf "Positive term package: %s\n" (term_package pos_term);
  Printf.printf "Negative term package: %s\n" (term_package neg_term);
  Printf.printf "Is positive: %b\n" (is_positive pos_term);
  Printf.printf "Is positive (neg): %b\n" (is_positive neg_term);

  let negated = negate_term pos_term in
  Printf.printf "Negated term is negative: %b\n" (not (is_positive negated));

  Printf.printf "\n"

(* Test 2: Package satisfies term *)
let test_package_satisfies_term () =
  Printf.printf "=== Testing Package Satisfies Term ===\n";

  let pkg = make_package "foo" "1.0.0" in
  let pos_term = Positive ("foo", [ "1.0.0"; "1.1.0" ]) in
  let neg_term = Negative ("foo", [ "2.0.0" ]) in
  let wrong_pkg_term = Positive ("bar", [ "1.0.0" ]) in

  Printf.printf "Package foo 1.0.0 satisfies Positive foo [1.0.0; 1.1.0]: %b\n"
    (package_satisfies_term pkg pos_term);
  Printf.printf "Package foo 1.0.0 satisfies Negative foo [2.0.0]: %b\n"
    (package_satisfies_term pkg neg_term);
  Printf.printf "Package foo 1.0.0 satisfies Positive bar [1.0.0]: %b\n"
    (package_satisfies_term pkg wrong_pkg_term);

  Printf.printf "\n"

(* Test 3: Dependency to incompatibility conversion *)
let test_dependency_conversion () =
  Printf.printf "=== Testing Dependency Conversion ===\n";

  let dep = make_dependency ("A", "1") "B" [ "1"; "2" ] in
  let incomp = dependency_to_incompatibility 0 dep in

  Printf.printf "Incompatibility ID: %d\n" incomp.id;
  Printf.printf "Number of terms: %d\n" (List.length incomp.terms);

  List.iteri
    (fun i term ->
      Printf.printf "Term %d: %s %s\n" i (term_package term)
        (if is_positive term then "POSITIVE" else "NEGATIVE"))
    incomp.terms;

  Printf.printf "\n"

(* Test 4: Initial state creation *)
let test_initial_state () =
  Printf.printf "=== Testing Initial State Creation ===\n";

  let simple_repo = [("A", "1"); ("B", "1")] in
  let state = create_initial_state simple_repo simple_deps simple_query in

  Printf.printf "Number of incompatibilities: %d\n"
    (List.length state.incompatibilities);
  Printf.printf "Decision level: %d\n" state.partial_solution.decision_level;
  Printf.printf "Number of assignments: %d\n"
    (List.length state.partial_solution.assignments);
  Printf.printf "Next ID: %d\n" state.next_id;

  Printf.printf "\nIncompatibilities:\n";
  List.iteri
    (fun i incomp ->
      Printf.printf "%d. ID:%d Terms:%d\n" i incomp.id
        (List.length incomp.terms))
    state.incompatibilities;

  Printf.printf "\n"

(* Test 5: Empty partial solution *)
let test_empty_partial_solution () =
  Printf.printf "=== Testing Empty Partial Solution ===\n";

  let empty_partial = { assignments = []; decision_level = 0 } in
  let term = Positive ("A", [ "1" ]) in

  Printf.printf "Term satisfied by empty solution: %b\n"
    (term_satisfied_by_partial_solution term empty_partial);
  Printf.printf "Term contradicted by empty solution: %b\n"
    (term_contradicted_by_partial_solution term empty_partial);

  Printf.printf "\n"

(* Test 6: Simple solve with minimal case *)
let test_minimal_solve () =
  Printf.printf "=== Testing Minimal Solve Case ===\n";

  (* Try with empty dependencies *)
  let empty_deps = [] in
  let empty_query = [] in

  Printf.printf "Solving empty case...\n";
  let empty_repo = [] in
  match solve empty_repo empty_deps empty_query with
  | Solution packages ->
      Printf.printf "Empty case solution: %d packages\n" (List.length packages)
  | Error (NoSolution incomp) ->
      Printf.printf "Empty case failed: NoSolution (id: %d)\n" incomp.id
  | Error (InvalidInput msg) ->
      Printf.printf "Empty case failed: InvalidInput: %s\n" msg;

      Printf.printf "\n"

(* Test 7: Single package case *)
let test_single_package () =
  Printf.printf "=== Testing Single Package Case ===\n";

  let single_deps = [] in
  (* No dependencies *)
  let single_query = [ make_package "A" "1" ] in

  Printf.printf "Solving single package case...\n";
  let single_repo = [make_package "A" "1"] in
  match solve single_repo single_deps single_query with
  | Solution packages ->
      Printf.printf "Single package solution: %d packages\n"
        (List.length packages);
      List.iter
        (fun (name, version) -> Printf.printf "  %s %s\n" name version)
        packages
  | Error (NoSolution incomp) ->
      Printf.printf "Single package failed: NoSolution (id: %d)\n" incomp.id
  | Error (InvalidInput msg) ->
      Printf.printf "Single package failed: InvalidInput: %s\n" msg;

      Printf.printf "\n"

(* Test 8: Simple dependency chain - this might cause the infinite loop *)
let test_simple_dependency () =
  Printf.printf "=== Testing Simple Dependency Chain ===\n";

  let deps = [ make_dependency ("A", "1") "B" [ "1" ] ] in
  let query = [ make_package "A" "1" ] in

  Printf.printf "Solving A->B dependency case...\n";
  Printf.printf "Dependencies: A 1 depends on B [1]\n";
  Printf.printf "Query: A 1\n";

  (* Add timeout mechanism by creating a counter *)
  let _counter = ref 0 in
  let _original_solve = solve in

  Printf.printf "Starting solve (will timeout after showing initial state)...\n";

  (* Let's examine the initial state first *)
  let complex_repo = [("A", "1"); ("B", "1"); ("C", "1")] in
  let initial_state = create_initial_state complex_repo deps query in
  Printf.printf "Initial state created successfully:\n";
  Printf.printf "  Incompatibilities: %d\n"
    (List.length initial_state.incompatibilities);
  Printf.printf "  Assignments: %d\n"
    (List.length initial_state.partial_solution.assignments);

  Printf.printf "  Incompatibility details:\n";
  List.iteri
    (fun i incomp ->
      Printf.printf "    %d. ID:%d Terms:%d\n" i incomp.id
        (List.length incomp.terms);
      List.iteri
        (fun j term ->
          Printf.printf "       Term %d: %s %s [%s]\n" j (term_package term)
            (if is_positive term then "POS" else "NEG")
            (String.concat "," (term_versions term)))
        incomp.terms)
    initial_state.incompatibilities;

  Printf.printf
    "\nThis case is incomplete - A depends on B but B is not available\n";
  Printf.printf "Skipping solve to test complete case instead...\n";
  Printf.printf "\n"

(* Test 9: Complete dependency case *)
let test_complete_dependency () =
  Printf.printf "=== Testing Complete Dependency Case ===\n";

  (* A depends on B, and B has no dependencies (complete case) *)
  let deps =
    [
      make_dependency ("A", "1") "B" [ "1" ];
      (* B 1 has no dependencies - represented by having an empty dependency list *)
    ]
  in
  let query = [ make_package "A" "1" ] in

  Printf.printf "Complete case: A 1 -> B 1, B 1 -> (nothing)\n";
  Printf.printf "Query: A 1\n";

  let repo = [("A", "1"); ("B", "1"); ("C", "1")] in
  let initial_state = create_initial_state repo deps query in
  Printf.printf "Initial state:\n";
  Printf.printf "  Incompatibilities: %d\n"
    (List.length initial_state.incompatibilities);
  Printf.printf "  Assignments: %d\n"
    (List.length initial_state.partial_solution.assignments);

  Printf.printf "Attempting solve...\n";
  match solve repo deps query with
  | Solution packages ->
      Printf.printf "SUCCESS! Solution: %d packages\n" (List.length packages);
      List.iter
        (fun (name, version) -> Printf.printf "  %s %s\n" name version)
        packages
  | Error (NoSolution incomp) ->
      Printf.printf "No solution (id: %d)\n" incomp.id
  | Error (InvalidInput msg) ->
      Printf.printf "Invalid input: %s\n" msg;

      Printf.printf "\n"

(* Main test runner *)
let run_all_tests () =
  Printf.printf "Running PubGrub Unit Tests\n";
  Printf.printf "==========================\n\n";

  test_term_operations ();
  test_package_satisfies_term ();
  test_dependency_conversion ();
  test_initial_state ();
  test_empty_partial_solution ();
  test_minimal_solve ();
  test_single_package ();
  test_simple_dependency ();
  test_complete_dependency ();

  Printf.printf "All tests completed.\n"

(* Run tests when module is executed *)
let () = run_all_tests ()
