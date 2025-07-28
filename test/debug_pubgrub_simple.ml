open Pac.Pubgrub

(* Simple debug version that just uses the main solve function *)
let debug_solve (repo : repository) (deps : dependencies) (query : query) : solve_result =
  Printf.printf "\n=== DEBUG SOLVE START ===\n";
  Printf.printf "Repository: %d packages\n" (List.length repo);
  Printf.printf "Dependencies: %d\n" (List.length deps);
  Printf.printf "Query: %d packages\n" (List.length query);
  
  let result = solve repo deps query in
  
  Printf.printf "\n=== DEBUG SOLVE END ===\n";
  result

let () =
  let simple_deps = [
    (("A", "1"), ("B", ["1"]));
  ] in
  let simple_repo = [("A", "1"); ("B", "1")] in
  let simple_query = [("A", "1")] in
  
  match debug_solve simple_repo simple_deps simple_query with
  | Solution packages ->
      Printf.printf "✓ SOLUTION: %s\n"
        (String.concat ", " (List.map (fun (n,v) -> n ^ " " ^ v) packages))
  | Error (NoSolution incomp) ->
      Printf.printf "✗ NO SOLUTION (id: %d)\n" incomp.id
  | Error (InvalidInput msg) ->
      Printf.printf "✗ INVALID INPUT: %s\n" msg