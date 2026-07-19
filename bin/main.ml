open Pac
open Cmdliner

let parse_instance filename =
  let ic = match filename with Some f -> open_in f | None -> In_channel.stdin in
  let lexbuf = Lexing.from_channel ic in
  let v =
    try Parser.instance Lexer.read lexbuf
    with e ->
      if ic <> In_channel.stdin then close_in ic;
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let col = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      Printf.eprintf "Parse error at line %d column %d token '%s'\n" line col tok;
      raise e
  in
  if ic <> In_channel.stdin then close_in ic;
  v

let parse_query str =
  let lexbuf = Lexing.from_string str in
  try Parser.query Lexer.read lexbuf
  with e ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let col = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    Printf.eprintf "Parse error at line %d column %d token '%s'\n" line col tok;
    raise e

let parse_packages str =
  let lexbuf = Lexing.from_string str in
  try Parser.packages Lexer.read lexbuf
  with e ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let col = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    Printf.eprintf "Parse error at line %d column %d token '%s'\n" line col tok;
    raise e

let parse_cmd filename =
  let instance = parse_instance filename in
  Ast.pp Format.std_formatter instance

let major_version = function
  | Core.Version v -> Core.Version (List.hd (String.split_on_char '.' v))
  | v -> v

let check_cmd filename query_str resolution_str calculus () =
  let instance = parse_instance filename in
  let _repo, deps = Core.of_ast instance in
  let g = major_version in
  let query = parse_query query_str in
  let deps =
    List.map
      (fun (n, vs) ->
        ( (Core.RootName, Core.RootVersion),
          (Core.Name n, List.map (fun v -> Core.Version v) vs) ))
      query
    @ deps
  in
  let resolution =
    (Core.RootName, Core.RootVersion)
    :: List.map
         (fun (n, v) -> (Core.Name n, Core.Version v))
         (parse_packages resolution_str)
  in
  match calculus with
  | "core" ->
      let open Core.Resolution in
      let root_inclusion = check_root_inclusion resolution in
      let dep_closure = check_dependency_closure deps resolution in
      let version_uniqueness = check_version_uniqueness resolution in
      let valid_resolution = check_resolution deps resolution in
      Printf.printf "Core resolution: %b\n" valid_resolution;
      Printf.printf "\tRoot inclusion: %b\n" root_inclusion;
      Printf.printf "\tDependency closure: %b\n" dep_closure;
      Printf.printf "\tVersion uniqueness: %b\n" version_uniqueness
  | "concurrent" ->
      let open Concurrent.Resolution in
      let root_inclusion = check_root_inclusion resolution in
      let dep_closure = check_dependency_closure deps resolution in
      let version_granularity = check_version_granularity g resolution in
      let concurrent_resolution = check_concurrent_resolution g deps resolution in
      Printf.printf "Concurrent resolution: %b\n" concurrent_resolution;
      Printf.printf "\tRoot inclusion: %b\n" root_inclusion;
      Printf.printf "\tDependency closure: %b\n" dep_closure;
      Printf.printf "\tVersion granularity: %b\n" version_granularity
  | _ ->
      failwith
        (Printf.sprintf
           "Unknown calculus: %s (expected 'core', 'concurrent', or 'pubgrub')" calculus)

let reduce_cmd filename granularity from_calculus to_calculus () =
  let instance = parse_instance filename in
  let core = Core.of_ast instance in
  let g =
    match granularity with
    | "major" -> major_version
    | custom -> failwith (Printf.sprintf "Unknown granularity: %s" custom)
  in
  match (from_calculus, to_calculus) with
  | "concurrent", "core" ->
      let reduced = Concurrent.encode g core in
      let ast_reduced = Core.to_ast reduced in
      Ast.pp Format.std_formatter ast_reduced
  | "core", "concurrent" -> Ast.pp Format.std_formatter instance
  | src, dst when src = dst -> Ast.pp Format.std_formatter instance
  | src, dst -> failwith (Printf.sprintf "Unsupported reduction: %s to %s" src dst)

module StringOrd = struct
  type t = string

  let compare = String.compare
  let pp = Format.pp_print_string
end

module Solver = Pubgrub.Make (StringOrd) (StringOrd)

let solve_cmd filename query_str debug () =
  let instance = parse_instance filename in
  let query = parse_query query_str in
  let repo, deps = Core.of_ast instance in
  let repo_tbl = Hashtbl.create 16 in
  List.iter
    (function Core.Name n, Core.Version v -> Hashtbl.add repo_tbl n v | _ -> ())
    repo;
  let dep_tbl = Hashtbl.create 16 in
  List.iter
    (function
      | (Core.Name n, Core.Version v), (Core.Name dn, dvs) ->
          let vs =
            List.filter_map (function Core.Version v -> Some v | _ -> None) dvs
          in
          Hashtbl.add dep_tbl (n, v) (dn, Solver.Ranges.of_list vs)
      | _ -> ())
    deps;
  let versions n = Hashtbl.find_all repo_tbl n in
  let dependencies n v = Hashtbl.find_all dep_tbl (n, v) in
  let query = List.map (fun (n, vs) -> (n, Solver.Ranges.of_list vs)) query in
  Pubgrub.set_debug debug;
  match Solver.solve ~versions ~dependencies query with
  | Ok resolution ->
      Format.printf "%a\n%!"
        Format.(
          pp_print_list
            ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
            (fun fmt (n, v) -> fprintf fmt "%s %s" n v))
        resolution
  | Error incomp -> Format.printf "%a\n%!" Solver.explain_incompatibility incomp

let file_arg =
  let doc = "Input file with dependency information (stdin used if not specified)" in
  Arg.(value & opt (some string) None & info [ "f"; "file" ] ~docv:"FILE" ~doc)

let granularity_arg =
  let doc = "Granularity function to use" in
  Arg.(value & opt string "major" & info [ "g"; "granularity" ] ~docv:"GRANULARITY" ~doc)

let calculus_arg =
  let doc = "Resolution calculus to use for verification" in
  Arg.(value & opt string "core" & info [ "c"; "calculus" ] ~docv:"CALCULUS" ~doc)

let query_arg =
  let doc = "In the format 'A ( 1 2 ... ) B ( 1 2 3 ... )'" in
  Arg.(required & opt (some string) None & info [ "q"; "query" ] ~docv:"QUERY" ~doc)

let resolution_arg =
  let doc =
    "Comma-separated list of packages in the resolution (format: 'name version')"
  in
  Arg.(
    required & opt (some string) None & info [ "r"; "resolution" ] ~docv:"RESOLUTION" ~doc)

let debug_arg =
  let doc = "Enable debug output" in
  Arg.(value & flag & info [ "d"; "debug" ] ~doc)

let parse_term = Term.(const parse_cmd $ file_arg)

let parse_info =
  Cmd.info "parse" ~doc:"Parse a dependency file and print its contents"
    ~man:
      [
        `S Manpage.s_description;
        `P "Parses a file containing dependencies and prints the parsed structure.";
      ]

let from_calculus_arg =
  let doc = "Source calculus for dependency reduction" in
  Arg.(value & opt string "concurrent" & info [ "from" ] ~docv:"FROM_CALCULUS" ~doc)

let to_calculus_arg =
  let doc = "Target calculus for dependency reduction" in
  Arg.(value & opt string "core" & info [ "to" ] ~docv:"TO_CALCULUS" ~doc)

let reduce_term =
  Term.(
    const reduce_cmd $ file_arg $ granularity_arg $ from_calculus_arg $ to_calculus_arg
    $ const ())

let reduce_info =
  Cmd.info "reduce" ~doc:"Reduce dependencies from one calculus to another"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Reduces dependencies from one calculus to another using the specified \
           granularity function.";
        `P "Use --from to specify the source calculus and --to for the target calculus.";
        `P
          "Currently supports reduction from concurrent to core. Reduction from core to \
           concurrent is not supported.";
      ]

let check_term =
  Term.(const check_cmd $ file_arg $ query_arg $ resolution_arg $ calculus_arg $ const ())

let check_info =
  Cmd.info "check" ~doc:"Check if a resolution is valid"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Checks if the provided resolution is valid for the given dependencies, query, \
           and granularity function.";
        `P "Use the -c/--calculus option to specify which calculus to use for validation.";
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
            "Concurrent Package Calculus - Enhanced dependency resolution with granular \
             version constraints" );
        `I
          ( "pubgrub",
            "PubGrub Algorithm - Advanced dependency resolution with conflict-driven \
             learning" );
        `S Manpage.s_examples;
        `P "Parse a dependency file:";
        `P "  $(mname) parse -f deps.txt";
        `P "Reduce dependencies from one calculus to another:";
        `P "  $(mname) reduce -f deps.txt -g major --from concurrent --to core";
        `P "Check if a resolution is valid:";
        `P
          "  $(mname) check -f deps.txt -q 'A 1.0.0' -r 'A 1.0.0,B 1.0.0,C 1.0.0' -c core";
        `P "Solve dependencies using PubGrub:";
        `P "  $(mname) solve -f deps.txt -q 'A 1.0.0'";
      ]

let solve_term = Term.(const solve_cmd $ file_arg $ query_arg $ debug_arg $ const ())

let solve_info =
  Cmd.info "solve" ~doc:"Solve dependencies using PubGrub algorithm"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Uses the PubGrub algorithm to find a valid resolution for the given \
           dependencies and query.";
        `P
          "This command finds a solution automatically without requiring a pre-computed \
           resolution.";
      ]

let debian_cmd file query_str debug () =
  let pkgs = Deb_packages.parse_file file in
  let inst = Debian.load pkgs in
  let query_atoms =
    Deb_packages.parse_dep_field query_str
    |> List.map (function
         | [ a ] -> a
         | _ -> failwith "alternatives are not supported in the query")
  in
  Pubgrub.set_debug debug;
  match Debian.solve inst query_atoms with
  | Ok solution -> (
      let resolution = Debian.decode solution in
      List.iter (fun (n, v) -> Format.printf "%s %s\n" n v) resolution;
      match Debian.check inst resolution with
      | [] -> Format.printf "check: ok\n%!"
      | errs ->
          List.iter (fun e -> Format.printf "check: %s\n" e) errs;
          Format.print_flush ();
          exit 2)
  | Error incomp ->
      Format.printf "%a\n%!" Debian.Solver.explain_incompatibility incomp;
      exit 1

let opam_cmd repo_dir query_str vars free with_test with_doc debug () =
  let repo = Opam_repo.load repo_dir in
  let sigma =
    List.map
      (fun kv ->
        match String.index_opt kv '=' with
        | Some i ->
            (String.sub kv 0 i, String.sub kv (i + 1) (String.length kv - i - 1))
        | None -> (kv, "true"))
      vars
    @ Opam_frontend.default_sigma
  in
  let root_overrides =
    (if with_test then [ ("with-test", "true") ] else [])
    @ if with_doc then [ ("with-doc", "true") ] else []
  in
  let cfg = { Opam_frontend.repo; sigma; free; root_overrides; overrides = [] } in
  let rel_of_deb : Deb_packages.rel -> Opam_repo.rel = function
    | Lt -> Lt | Le -> Le | Eq -> Eq | Ge -> Ge | Gt -> Gt
  in
  let query =
    Deb_packages.parse_dep_field query_str
    |> List.map (function
         | [ (a : Deb_packages.atom) ] ->
             ( a.dep_name,
               Option.map
                 (fun (r, v) -> (rel_of_deb r, Deb_version.to_string v))
                 a.dep_rel )
         | _ -> failwith "alternatives are not supported in the query")
  in
  Pubgrub.set_debug debug;
  match Opam_frontend.solve cfg query with
  | Ok solution -> (
      let pkgs, free_choices = Opam_frontend.decode cfg solution in
      List.iter (fun (x, v) -> Format.printf "%s = %s\n" x v) free_choices;
      List.iter (fun (n, v) -> Format.printf "%s.%s\n" n v) pkgs;
      let scoped_roots =
        if root_overrides = [] then [] else List.map fst query
      in
      match Opam_frontend.check ~scoped_roots cfg pkgs free_choices with
      | [] -> Format.printf "check: ok\n%!"
      | errs ->
          List.iter (fun e -> Format.printf "check: %s\n" e) errs;
          Format.print_flush ();
          exit 2)
  | Error incomp ->
      Format.printf "%a\n%!" Opam_frontend.Solver.explain_incompatibility incomp;
      exit 1

let opam_repo_arg =
  let doc = "opam repository directory (containing packages/)" in
  Arg.(required & opt (some dir) None & info [ "r"; "repo" ] ~docv:"DIR" ~doc)

let opam_var_arg =
  let doc = "Set a variable, e.g. --var os=macos or --var with-test (=true)" in
  Arg.(value & opt_all string [] & info [ "var" ] ~docv:"K=V" ~doc)

let opam_free_arg =
  let doc = "Leave a variable free for the solver to choose" in
  Arg.(value & opt_all string [] & info [ "free" ] ~docv:"VAR" ~doc)

let cargo_cmd index_dir query_str vars free dev debug () =
  let index = Cargo_index.create index_dir in
  let sigma =
    List.map
      (fun kv ->
        match String.index_opt kv '=' with
        | Some i -> (String.sub kv 0 i, String.sub kv (i + 1) (String.length kv - i - 1))
        | None -> (kv, "true"))
      vars
    @ Cargo_frontend.default_sigma
  in
  let cfg = { Cargo_frontend.index; sigma; free; links_seen = Hashtbl.create 16 } in
  let query =
    String.split_on_char ',' query_str
    |> List.map String.trim
    |> List.filter (( <> ) "")
    |> List.map (fun atom ->
           let tokens =
             String.split_on_char ' ' atom |> List.map String.trim |> List.filter (( <> ) "")
           in
           match tokens with
           | name :: rest ->
               let feats, reqs =
                 List.partition (String.starts_with ~prefix:"+") rest
               in
               {
                 Cargo_frontend.q_name = name;
                 q_req =
                   (match reqs with
                   | [] -> Semver.parse_req "*"
                   | _ -> Semver.parse_req (String.concat "," reqs));
                 q_feats =
                   List.map (fun f -> String.sub f 1 (String.length f - 1)) feats;
               }
           | [] -> failwith "empty query atom")
  in
  Pubgrub.set_debug debug;
  match Cargo_frontend.solve ~dev cfg query with
  | Ok solution -> (
      let pkgs, feats_of, free_choices = Cargo_frontend.decode cfg solution in
      List.iter (fun (x, v) -> Format.printf "%s = %s\n" x v) free_choices;
      List.iter
        (fun (n, v) ->
          match feats_of (n, v) with
          | [] -> Format.printf "%s %s\n" n v
          | fs -> Format.printf "%s %s (%s)\n" n v (String.concat " " fs))
        pkgs;
      Format.printf "crates parsed: %d\n" (Cargo_index.crates_parsed index);
      let dev_roots =
        if dev then List.map (fun a -> a.Cargo_frontend.q_name) query else []
      in
      match Cargo_frontend.check ~dev_roots cfg pkgs feats_of free_choices with
      | [] -> Format.printf "check: ok\n%!"
      | errs ->
          List.iter (fun e -> Format.printf "check: %s\n" e) errs;
          Format.print_flush ();
          exit 2)
  | Error incomp ->
      Format.printf "%a\n%!" Cargo_frontend.Solver.explain_incompatibility incomp;
      exit 1

let cargo_semver_cmd v req () =
  print_endline
    (if Semver.matches (Semver.parse v) (Semver.parse_req req) then "match" else "no-match")

let apk_cmd file query_str debug () =
  let inst = Apk_index.load file in
  let query =
    String.split_on_char ',' query_str
    |> List.map String.trim
    |> List.filter (( <> ) "")
    |> List.map (fun s ->
           let a = Apk_index.parse_atom s in
           if a.a_neg then failwith "negative atoms are not supported in the query" else a)
  in
  Pubgrub.set_debug debug;
  match Apk_frontend.solve inst query with
  | Ok solution -> (
      let resolution = Apk_frontend.decode solution in
      List.iter (fun (n, v) -> Format.printf "%s %s\n" n v) resolution;
      match Apk_frontend.check inst resolution with
      | [] -> Format.printf "check: ok\n%!"
      | errs ->
          List.iter (fun e -> Format.printf "check: %s\n" e) errs;
          Format.print_flush ();
          exit 2)
  | Error incomp ->
      Format.printf "%a\n%!" Apk_frontend.Solver.explain_incompatibility incomp;
      exit 1

let apk_compare_cmd v1 v2 () =
  let c = Apk_version.compare (Apk_version.parse v1) (Apk_version.parse v2) in
  print_endline (if c < 0 then "lt" else if c > 0 then "gt" else "eq")

let deb_compare_cmd v1 v2 () =
  let c = Deb_version.compare (Deb_version.parse v1) (Deb_version.parse v2) in
  print_endline (if c < 0 then "lt" else if c > 0 then "gt" else "eq")

let packages_file_arg =
  let doc = "Debian Packages file" in
  Arg.(required & opt (some string) None & info [ "f"; "file" ] ~docv:"FILE" ~doc)

let debian_query_arg =
  let doc = "Install request, in Depends syntax (e.g. 'curl, postfix (>= 3.7)')" in
  Arg.(required & opt (some string) None & info [ "q"; "query" ] ~docv:"QUERY" ~doc)

let debian_term =
  Term.(const debian_cmd $ packages_file_arg $ debian_query_arg $ debug_arg $ const ())

let debian_info =
  Cmd.info "debian"
    ~doc:"Resolve a Debian install request by reduction to the core calculus"

let ver_arg n docv = Arg.(required & pos n (some string) None & info [] ~docv)

let deb_compare_term =
  Term.(const deb_compare_cmd $ ver_arg 0 "V1" $ ver_arg 1 "V2" $ const ())

let deb_compare_info =
  Cmd.info "deb-compare" ~doc:"Compare two Debian version strings (prints lt/eq/gt)"

let opam_with_test_arg =
  let doc = "Enable with-test for the queried packages only (never transitive)" in
  Arg.(value & flag & info [ "with-test" ] ~doc)

let opam_with_doc_arg =
  let doc = "Enable with-doc for the queried packages only (never transitive)" in
  Arg.(value & flag & info [ "with-doc" ] ~doc)

let opam_term =
  Term.(
    const opam_cmd $ opam_repo_arg $ debian_query_arg $ opam_var_arg $ opam_free_arg
    $ opam_with_test_arg $ opam_with_doc_arg
    $ debug_arg $ const ())

let opam_info =
  Cmd.info "opam" ~doc:"Resolve an opam install request by reduction to the core calculus"

let cargo_index_arg =
  let doc = "crates.io index directory" in
  Arg.(required & opt (some dir) None & info [ "r"; "repo" ] ~docv:"DIR" ~doc)

let cargo_dev_arg =
  let doc = "Include dev-dependencies of the queried crates (never transitive)" in
  Arg.(value & flag & info [ "dev" ] ~doc)

let cargo_term =
  Term.(
    const cargo_cmd $ cargo_index_arg $ debian_query_arg $ opam_var_arg $ opam_free_arg
    $ cargo_dev_arg $ debug_arg $ const ())

let cargo_info =
  Cmd.info "cargo" ~doc:"Resolve a cargo install request by reduction to the core calculus"

let sv_arg n docv = Arg.(required & pos n (some string) None & info [] ~docv)

let cargo_semver_term = Term.(const cargo_semver_cmd $ sv_arg 0 "VERSION" $ sv_arg 1 "REQ" $ const ())

let cargo_semver_info =
  Cmd.info "semver-match" ~doc:"Test a semver version against a cargo requirement"

let apk_file_arg =
  let doc = "APKINDEX file (extracted)" in
  Arg.(required & opt (some string) None & info [ "f"; "file" ] ~docv:"FILE" ~doc)

let apk_term = Term.(const apk_cmd $ apk_file_arg $ debian_query_arg $ debug_arg $ const ())

let apk_info =
  Cmd.info "apk" ~doc:"Resolve an Alpine install request by reduction to the core calculus"

let apk_compare_term =
  Term.(const apk_compare_cmd $ ver_arg 0 "V1" $ ver_arg 1 "V2" $ const ())

let apk_compare_info =
  Cmd.info "apk-compare" ~doc:"Compare two apk version strings (prints lt/eq/gt)"

let () =
  let cmds =
    [
      Cmd.v parse_info parse_term;
      Cmd.v reduce_info reduce_term;
      Cmd.v check_info check_term;
      Cmd.v solve_info solve_term;
      Cmd.v debian_info debian_term;
      Cmd.v deb_compare_info deb_compare_term;
      Cmd.v opam_info opam_term;
      Cmd.v cargo_info cargo_term;
      Cmd.v cargo_semver_info cargo_semver_term;
      Cmd.v apk_info apk_term;
      Cmd.v apk_compare_info apk_compare_term;
    ]
  in
  let cmd = Cmd.group default_info cmds in
  exit (Cmd.eval cmd)
