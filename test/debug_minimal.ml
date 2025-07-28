let debug_minimal () =
  Printf.printf "=== TESTING MINIMAL EMPTY DEPS ===\n\n";

  let ic = open_in "test/test_minimal.pac" in
  let ast_deps =
    Pac.Parser.expression Pac.Lexer.read (Lexing.from_channel ic)
  in
  close_in ic;

  Printf.printf "File contains: C 1 ( )\n";
  Printf.printf "AST:\n";
  Format.printf "%a\n" Pac.Ast.pp ast_deps;

  let deps = Pac.of_ast_expression ast_deps in
  Printf.printf "Parsed dependencies: %d\n" (List.length deps);

  Printf.printf "\n=== TEST COMPLETE ===\n"

let () = debug_minimal ()
