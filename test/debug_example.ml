open Pac.Pubgrub

let debug_example () =
  Printf.printf "=== DEBUGGING EXAMPLE.PAC ===\n\n";

  (* Parse example.pac *)
  let ic = open_in "examples/example.pac" in
  let ast_deps =
    Pac.Parser.expression Pac.Lexer.read (Lexing.from_channel ic)
  in
  close_in ic;

  Printf.printf "Raw AST dependencies count: (need to inspect AST structure)\n";

  let deps = Pac.of_ast_expression ast_deps in
  let repo = Pac.repository_from_ast ast_deps in
  let query = [ ("A", "1") ] in

  Printf.printf "Dependencies parsed: %d\n" (List.length deps);
  List.iteri
    (fun i (depender, (target_name, target_versions)) ->
      Printf.printf "%d. %s %s depends on %s [%s]\n" i (fst depender)
        (snd depender) target_name
        (String.concat "," target_versions))
    deps;

  Printf.printf "\nRaw dependency structure:\n";
  List.iteri
    (fun i dep ->
      Printf.printf "%d. Depender: (%s, %s), Target: (%s, [%s])\n" i
        (fst (fst dep))
        (snd (fst dep))
        (fst (snd dep))
        (String.concat ";" (snd (snd dep))))
    deps;

  Printf.printf "\nAvailable packages:\n";
  List.iter (fun (name, version) -> Printf.printf "  %s %s\n" name version) repo;

  Printf.printf "\nIncompatibilities:\n";
  let dep_incomps = dependencies_to_incompatibilities deps in
  List.iteri
    (fun i incomp ->
      Printf.printf "%d. Terms: " i;
      List.iter
        (fun term ->
          Printf.printf "[%s %s %s] " (term_package term)
            (if is_positive term then "MUST" else "MUST_NOT")
            (String.concat "," (term_versions term)))
        incomp.terms;
      Printf.printf "\n")
    dep_incomps;

  Printf.printf "\nPubGrub solve result:\n";
  match solve repo deps query with
  | Solution packages ->
      Printf.printf "✓ SOLUTION FOUND:\n";
      List.iter
        (fun (name, version) -> Printf.printf "  %s %s\n" name version)
        packages
  | Error (NoSolution incomp) ->
      Printf.printf "✗ NO SOLUTION (incompatibility ID: %d)\n" incomp.id;
      Printf.printf "Incompatibility terms: %d\n" (List.length incomp.terms);
      List.iteri
        (fun i term ->
          Printf.printf "  %d. %s %s [%s]\n" i (term_package term)
            (if is_positive term then "MUST" else "MUST_NOT")
            (String.concat "," (term_versions term)))
        incomp.terms;
      Printf.printf "Cause: %s\n"
        (match incomp.cause with
        | External ext_cause -> "External: " ^ (format_external_cause ext_cause)
        | Derived (_, _) -> "Derived from conflict resolution")
  | Error (InvalidInput msg) ->
      Printf.printf "✗ INVALID INPUT: %s\n" msg;

      Printf.printf "\n=== DEBUG COMPLETE ===\n"

let () = debug_example ()
