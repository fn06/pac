open Pac.Pubgrub

let test_case name pac_file query expected_result =
  Printf.printf "=== %s ===\n" name;

  (* Parse the .pac file manually *)
  let ic = open_in pac_file in
  let ast_deps =
    Pac.Parser.expression Pac.Lexer.read (Lexing.from_channel ic)
  in
  close_in ic;
  let deps = Pac.of_ast_expression ast_deps in
  let repo = Pac.repository_from_ast ast_deps in

  Printf.printf "Dependencies parsed: %d\n" (List.length deps);
  Printf.printf "Query: %s\n"
    (String.concat ", " (List.map (fun (n, v) -> n ^ " " ^ v) query));

  Printf.printf "\nTesting with Core resolver:\n";
  let test_resolution = query in
  (* Simple test - try to resolve with just query packages *)
  let core_valid =
    Pac.Resolution.check_resolution deps ~query ~resolution:test_resolution
  in
  Printf.printf "Core resolution valid: %b\n" core_valid;

  Printf.printf "\nTesting with PubGrub resolver:\n";
  match solve repo deps query with
  | Solution packages ->
      Printf.printf "✓ SOLUTION FOUND:\n";
      List.iter
        (fun (name, version) -> Printf.printf "  %s %s\n" name version)
        packages;
      Printf.printf "Expected: %s\n" expected_result
  | Error (NoSolution incomp) ->
      Printf.printf "✗ NO SOLUTION (incompatibility ID: %d)\n" incomp.id;
      Printf.printf "Incompatibility terms: %d\n" (List.length incomp.terms);
      List.iteri
        (fun i term ->
          Printf.printf "  %d. %s %s [%s]\n" i (term_package term)
            (if is_positive term then "MUST" else "MUST NOT")
            (String.concat "," (term_versions term)))
        incomp.terms;
      Printf.printf "Cause: %s\n"
        (match incomp.cause with
        | External msg -> "External: " ^ msg
        | Derived (_, _) -> "Derived from conflict resolution");
      Printf.printf "Expected: %s\n" expected_result
  | Error (InvalidInput msg) ->
      Printf.printf "✗ INVALID INPUT: %s\n" msg;
      Printf.printf "Expected: %s\n" expected_result;

      Printf.printf "\n"

let run_pathological_tests () =
  Printf.printf "🧪 PATHOLOGICAL TEST CASES\n";
  Printf.printf "Testing PubGrub error reporting capabilities\n\n";

  (* Test 1: Complex constraint satisfaction *)
  test_case "Complex Constraint Satisfaction" "test/pathological_cases.pac"
    [ ("A", "1") ]
    "Should find solution with careful D version selection";

  (* Test 2: Impossible constraints *)
  test_case "Impossible Version Constraints" "test/impossible_case.pac"
    [ ("A", "1") ]
    "Should fail with clear explanation of B->D vs C->D conflict";

  (* Test 3: Missing dependency *)
  test_case "Missing Dependency" "test/missing_dependency.pac"
    [ ("A", "1") ]
    "Should fail because B 1 is not available";

  (* Test 4: Circular dependencies *)
  test_case "Circular Dependencies" "test/circular_deps.pac"
    [ ("A", "1") ]
    "Should either resolve or detect the cycle";

  Printf.printf "🎯 PATHOLOGICAL TESTS COMPLETE\n"

let () = run_pathological_tests ()
