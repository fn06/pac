open Pac
open Cmdliner

let process_file filename =
  let ic =
    match filename with Some f -> open_in f | None -> In_channel.stdin
  in
  try
    let v = Parser.expression Lexer.read (Lexing.from_channel ic) in
    if ic <> In_channel.stdin then close_in ic;
    v
  with e ->
    if ic <> In_channel.stdin then close_in ic;
    raise e

let string_of_package (name, version) = Printf.sprintf "%s %s" name version

let parse_package str =
  match String.split_on_char ' ' str with
  | [ name; version ] -> (name, version)
  | _ ->
      failwith
        (Printf.sprintf "Invalid package format: %s (expected 'name version')"
           str)

let parse_cmd filename =
  let deps = process_file filename in
  Ast.pp Format.std_formatter deps

let major_version v = List.hd (String.split_on_char '.' v)

let check_cmd filename query_str resolution_str granularity calculus () =
  let ast_deps = process_file filename in
  let deps = of_ast_expression ast_deps in
  let g =
    match granularity with
    | "major" -> major_version
    | custom -> failwith (Printf.sprintf "Unknown granularity: %s" custom)
  in
  let query = List.map parse_package (String.split_on_char ',' query_str) in
  let resolution =
    List.map parse_package (String.split_on_char ',' resolution_str)
  in
  Printf.printf "Query: %s\n"
    (String.concat ", " (List.map string_of_package query));
  Printf.printf "Resolution: %s\n"
    (String.concat ", " (List.map string_of_package resolution));
  match calculus with
  | "core" ->
      let query_inclusion =
        Resolution.check_query_inclusion ~query ~resolution
      in
      let dep_closure = Resolution.check_dependency_closure deps resolution in
      let version_uniqueness = Resolution.check_version_uniqueness resolution in
      let valid_resolution =
        Resolution.check_resolution deps ~query ~resolution
      in
      Printf.printf "Core resolution:\n";
      Printf.printf "\tQuery inclusion: %b\n" query_inclusion;
      Printf.printf "\tDependency closure: %b\n" dep_closure;
      Printf.printf "\tVersion uniqueness: %b\n" version_uniqueness;
      Printf.printf "\tValid core resolution: %b\n" valid_resolution
  | "concurrent" ->
      let query_inclusion =
        ConcurrentResolution.check_query_inclusion ~query ~resolution
      in
      let dep_closure =
        ConcurrentResolution.check_dependency_closure deps resolution
      in
      let version_granularity =
        ConcurrentResolution.check_version_granularity g resolution
      in
      let concurrent_resolution =
        ConcurrentResolution.check_concurrent_resolution g deps ~query
          ~resolution
      in
      Printf.printf "Concurrent resolution:\n";
      Printf.printf "\tQuery inclusion: %b\n" query_inclusion;
      Printf.printf "\tDependency closure: %b\n" dep_closure;
      Printf.printf "\tVersion granularity: %b\n" version_granularity;
      Printf.printf "\tValid concurrent resolution: %b\n" concurrent_resolution
  | "pubgrub" -> (
      (* Use PubGrub solver *)
      match Pubgrub.solve deps query with
      | Pubgrub.Solution solution ->
          Printf.printf "PubGrub resolution:\n";
          Printf.printf "\tSolution found: %s\n"
            (String.concat ", " (List.map string_of_package solution));
          Printf.printf "\tMatches provided resolution: %b\n"
            (List.for_all (fun pkg -> List.mem pkg solution) resolution
            && List.for_all (fun pkg -> List.mem pkg resolution) solution)
      | Pubgrub.Error (Pubgrub.NoSolution incomp) ->
          Printf.printf "PubGrub resolution:\n";
          Printf.printf "\tNo solution exists (incompatibility id: %d)\n"
            incomp.id
      | Pubgrub.Error (Pubgrub.InvalidInput msg) ->
          Printf.printf "PubGrub resolution:\n";
          Printf.printf "\tInvalid input: %s\n" msg)
  | _ ->
      failwith
        (Printf.sprintf
           "Unknown calculus: %s (expected 'core', 'concurrent', or 'pubgrub')"
           calculus)

let reduce_cmd filename granularity from_calculus to_calculus () =
  let ast_deps = process_file filename in
  let deps = of_ast_expression ast_deps in
  let g =
    match granularity with
    | "major" -> major_version
    | custom -> failwith (Printf.sprintf "Unknown granularity: %s" custom)
  in
  match (from_calculus, to_calculus) with
  | "concurrent", "core" ->
      let reduced = Concurrent.encode_dependencies g deps in
      let ast_reduced = to_ast_expression reduced in
      Ast.pp Format.std_formatter ast_reduced
  | "core", "concurrent" -> Ast.pp Format.std_formatter ast_deps
  | src, dst when src = dst -> Ast.pp Format.std_formatter ast_deps
  | src, dst ->
      failwith (Printf.sprintf "Unsupported reduction: %s to %s" src dst)

let file_arg =
  let doc =
    "Input file with dependency information (stdin used if not specified)"
  in
  Arg.(value & opt (some string) None & info [ "f"; "file" ] ~docv:"FILE" ~doc)

let granularity_arg =
  let doc = "Granularity function to use" in
  Arg.(
    value & opt string "major"
    & info [ "g"; "granularity" ] ~docv:"GRANULARITY" ~doc)

let calculus_arg =
  let doc = "Resolution calculus to use for verification" in
  Arg.(
    value & opt string "core" & info [ "c"; "calculus" ] ~docv:"CALCULUS" ~doc)

let query_arg =
  let doc =
    "Comma-separated list of packages in the query (format: 'name version')"
  in
  Arg.(
    required & opt (some string) None & info [ "q"; "query" ] ~docv:"QUERY" ~doc)

let resolution_arg =
  let doc =
    "Comma-separated list of packages in the resolution (format: 'name \
     version')"
  in
  Arg.(
    required
    & opt (some string) None
    & info [ "r"; "resolution" ] ~docv:"RESOLUTION" ~doc)

let parse_term = Term.(const parse_cmd $ file_arg)

let parse_info =
  Cmd.info "parse" ~doc:"Parse a dependency file and print its contents"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Parses a file containing dependencies and prints the parsed \
           structure.";
      ]

let from_calculus_arg =
  let doc = "Source calculus for dependency reduction" in
  Arg.(
    value & opt string "concurrent" & info [ "from" ] ~docv:"FROM_CALCULUS" ~doc)

let to_calculus_arg =
  let doc = "Target calculus for dependency reduction" in
  Arg.(value & opt string "core" & info [ "to" ] ~docv:"TO_CALCULUS" ~doc)

let reduce_term =
  Term.(
    const reduce_cmd $ file_arg $ granularity_arg $ from_calculus_arg
    $ to_calculus_arg $ const ())

let reduce_info =
  Cmd.info "reduce" ~doc:"Reduce dependencies from one calculus to another"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Reduces dependencies from one calculus to another using the \
           specified granularity function.";
        `P
          "Use --from to specify the source calculus and --to for the target \
           calculus.";
        `P
          "Currently supports reduction from concurrent to core. Reduction \
           from core to concurrent is not supported.";
      ]

let check_term =
  Term.(
    const check_cmd $ file_arg $ query_arg $ resolution_arg $ granularity_arg
    $ calculus_arg $ const ())

let check_info =
  Cmd.info "check" ~doc:"Check if a resolution is valid"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Checks if the provided resolution is valid for the given \
           dependencies, query, and granularity function.";
        `P
          "Use the -c/--calculus option to specify which calculus to use for \
           validation.";
      ]

let default_info =
  Cmd.info "pac" ~version:"1.0.0" ~doc:"Package dependency resolution tool"
    ~man:
      [
        `S Manpage.s_description;
        `P "A tool for working with package dependencies and resolutions.";
        `S "SUPPORTED CALCULI";
        `P "The following calculi are supported by this tool:";
        `I
          ( "core",
            "Package Calculus - Standard dependency resolution without version \
             constraints" );
        `I
          ( "concurrent",
            "Concurrent Package Calculus - Enhanced dependency resolution with \
             granular version constraints" );
        `I
          ( "pubgrub",
            "PubGrub Algorithm - Advanced dependency resolution with \
             conflict-driven learning" );
        `S Manpage.s_examples;
        `P "Parse a dependency file:";
        `P "  $(mname) parse -f deps.txt";
        `P "Reduce dependencies from one calculus to another:";
        `P "  $(mname) reduce -f deps.txt -g major --from concurrent --to core";
        `P "Check if a resolution is valid:";
        `P
          "  $(mname) check -f deps.txt -q 'A 1.0.0' -r 'A 1.0.0,B 1.0.0,C \
           1.0.0' -c core";
        `P "Solve dependencies using PubGrub:";
        `P "  $(mname) solve -f deps.txt -q 'A 1.0.0'";
      ]

let solve_cmd filename query_str () =
  let ast_deps = process_file filename in
  let deps = of_ast_expression ast_deps in
  let query = List.map parse_package (String.split_on_char ',' query_str) in

  Printf.printf "Query: %s\n"
    (String.concat ", " (List.map string_of_package query));

  match Pubgrub.solve deps query with
  | Pubgrub.Solution solution ->
      Printf.printf "Solution found:\n";
      List.iter
        (fun (name, version) -> Printf.printf "  %s %s\n" name version)
        solution
  | Pubgrub.Error (Pubgrub.NoSolution incomp) ->
      Printf.printf "No solution exists (incompatibility id: %d)\n" incomp.id
  | Pubgrub.Error (Pubgrub.InvalidInput msg) ->
      Printf.printf "Invalid input: %s\n" msg

let solve_term = Term.(const solve_cmd $ file_arg $ query_arg $ const ())

let solve_info =
  Cmd.info "solve" ~doc:"Solve dependencies using PubGrub algorithm"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Uses the PubGrub algorithm to find a valid resolution for the given \
           dependencies and query.";
        `P
          "This command finds a solution automatically without requiring a \
           pre-computed resolution.";
      ]

let () =
  let cmds =
    [
      Cmd.v parse_info parse_term;
      Cmd.v reduce_info reduce_term;
      Cmd.v check_info check_term;
      Cmd.v solve_info solve_term;
    ]
  in
  let cmd = Cmd.group default_info cmds in
  exit (Cmd.eval cmd)
