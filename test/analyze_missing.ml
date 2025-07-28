open Pac.Pubgrub

let analyze_missing_dependency () =
  Printf.printf "=== ANALYZING MISSING DEPENDENCY BUG ===\n\n";

  (* Parse missing_dependency.pac *)
  let ic = open_in "test/missing_dependency.pac" in
  let ast_deps =
    Pac.Parser.expression Pac.Lexer.read (Lexing.from_channel ic)
  in
  close_in ic;

  Printf.printf
    "Raw file should have 2 lines: 'A 1 ( B ( 1 ) )' and 'C 1 ( )'\n";
  Printf.printf "AST:\n";
  Format.printf "%a\n" Pac.Ast.pp ast_deps;

  let deps = Pac.of_ast_expression ast_deps in
  let repo = Pac.repository_from_ast ast_deps in
  let query = [ ("A", "1") ] in

  Printf.printf "File contents:\n";
  Printf.printf "A 1 ( B ( 1 ) )\n";
  Printf.printf "C 1 ( )\n\n";

  Printf.printf "Parsed dependencies: %d\n" (List.length deps);
  List.iteri
    (fun i (depender, (target_name, target_versions)) ->
      Printf.printf "%d. %s %s depends on %s [%s]\n" i (fst depender)
        (snd depender) target_name
        (String.concat "," target_versions))
    deps;

  Printf.printf "\nAvailable packages (from repository):\n";
  List.iter (fun (name, version) -> Printf.printf "  %s %s\n" name version) repo;

  Printf.printf "\nAll packages mentioned (including targets):\n";
  let all_mentioned =
    List.fold_left
      (fun acc (depender, (target_name, target_versions)) ->
        let depender_pkg = depender in
        let target_pkgs =
          List.map (fun v -> (target_name, v)) target_versions
        in
        depender_pkg :: (target_pkgs @ acc))
      [] deps
  in
  let unique_packages = List.sort_uniq compare all_mentioned in
  List.iter
    (fun (name, version) -> Printf.printf "  %s %s\n" name version)
    unique_packages;

  Printf.printf
    "\n\
     The problem: B 1 is referenced as a dependency but never defined as an \
     available package!\n";
  Printf.printf "PubGrub should detect this and fail.\n\n";

  Printf.printf "\nDebugging incompatibilities:\n";
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

  Printf.printf "\nCurrent PubGrub result:\n";
  match solve repo deps query with
  | Solution packages ->
      Printf.printf "❌ INCORRECTLY found solution:\n";
      List.iter
        (fun (name, version) -> Printf.printf "  %s %s\n" name version)
        packages;
      Printf.printf "\nThis is wrong - B 1 doesn't exist!\n"
  | Error _err ->
      Printf.printf "✅ Correctly failed\n";

      Printf.printf "\n=== ANALYSIS COMPLETE ===\n"

let () = analyze_missing_dependency ()
