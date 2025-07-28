let debug_empty () =
  Printf.printf "=== DEBUGGING EMPTY DEPENDENCIES ===\n\n";

  (* Parse simple case *)
  let ic = open_in "test/simple_empty_deps.pac" in
  let ast_deps =
    Pac.Parser.expression Pac.Lexer.read (Lexing.from_channel ic)
  in
  close_in ic;
  let deps = Pac.of_ast_expression ast_deps in

  Printf.printf "File contains: X 1 ( )\n";
  Printf.printf "Parsed dependencies: %d\n" (List.length deps);

  List.iteri
    (fun i (depender, (target_name, target_versions)) ->
      Printf.printf "%d. Depender: (%s, %s), Target: (%s, [%s])\n" i
        (fst depender) (snd depender) target_name
        (String.concat ";" target_versions))
    deps;

  let repo = Pac.repository_from_ast ast_deps in
  Printf.printf "\nAvailable packages: %d\n" (List.length repo);
  List.iter (fun (name, version) -> Printf.printf "  %s %s\n" name version) repo;

  Printf.printf "\n=== DEBUG COMPLETE ===\n"

let () = debug_empty ()
