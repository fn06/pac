(* Parser for Debian Packages files (RFC-822-style stanzas). *)

type rel = Lt | Le | Eq | Ge | Gt

type atom = {
  dep_name : string;
  dep_rel : (rel * Deb_version.t) option;
  dep_rel_str : string; (* canonical constraint, e.g. ">= 1.2-1"; "" if none *)
}

type pkg = {
  name : string;
  version : Deb_version.t;
  version_str : string;
  depends : atom list list; (* conjunction of alternative groups *)
  conflicts : atom list; (* Conflicts + Breaks; no alternatives allowed *)
  provides : (string * Deb_version.t option) list;
}

let satisfies v = function
  | None -> true
  | Some (rel, w) -> (
      let c = Deb_version.compare v w in
      match rel with
      | Lt -> c < 0
      | Le -> c <= 0
      | Eq -> c = 0
      | Ge -> c >= 0
      | Gt -> c > 0)

let strip s =
  let n = String.length s in
  let ws c = c = ' ' || c = '\t' in
  let rec start i = if i < n && ws s.[i] then start (i + 1) else i in
  let rec stop j i = if j >= i && ws s.[j] then stop (j - 1) i else j in
  let i = start 0 in
  let j = stop (n - 1) i in
  String.sub s i (j - i + 1)

let split_on c s = String.split_on_char c s |> List.map strip |> List.filter (( <> ) "")

let parse_rel s =
  match s with
  | "<<" | "<" -> Lt
  | "<=" -> Le
  | "=" -> Eq
  | ">=" -> Ge
  | ">>" | ">" -> Gt
  | _ -> failwith ("unknown version relation: " ^ s)

let rel_to_string = function
  | Lt -> "<<"
  | Le -> "<="
  | Eq -> "="
  | Ge -> ">="
  | Gt -> ">>"

(* "name[:arch] [(op ver)]", ignoring any "[arch]" / "<profile>" qualifiers. *)
let parse_atom s =
  let s = strip s in
  let cut_at cs str =
    let idx =
      List.filter_map (fun c -> String.index_opt str c) cs
      |> List.fold_left min (String.length str)
    in
    strip (String.sub str 0 idx)
  in
  let name_part, rel =
    match String.index_opt s '(' with
    | Some i ->
        let close =
          match String.index_from_opt s i ')' with
          | Some j -> j
          | None -> failwith ("unclosed version relation in: " ^ s)
        in
        let inside = strip (String.sub s (i + 1) (close - i - 1)) in
        let rel_str, ver_str =
          match String.index_opt inside ' ' with
          | Some k ->
              ( strip (String.sub inside 0 k),
                strip (String.sub inside k (String.length inside - k)) )
          | None ->
              (* dpkg also accepts e.g. "(>=1.2)" without a space *)
              let rel_char c = c = '<' || c = '>' || c = '=' in
              let rec oplen k =
                if k < String.length inside && rel_char inside.[k] then oplen (k + 1) else k
              in
              let k = oplen 0 in
              (String.sub inside 0 k, strip (String.sub inside k (String.length inside - k)))
        in
        (strip (String.sub s 0 i), Some (parse_rel rel_str, ver_str))
    | None -> (cut_at [ '['; '<' ] s, None)
  in
  let name_part = cut_at [ '['; '<' ] name_part in
  let dep_name =
    match String.index_opt name_part ':' with
    | Some i -> String.sub name_part 0 i
    | None -> name_part
  in
  match rel with
  | None -> { dep_name; dep_rel = None; dep_rel_str = "" }
  | Some (r, vs) ->
      {
        dep_name;
        dep_rel = Some (r, Deb_version.parse vs);
        dep_rel_str = rel_to_string r ^ " " ^ vs;
      }

let parse_dep_field s = split_on ',' s |> List.map (fun g -> split_on '|' g |> List.map parse_atom)

let parse_provides_field s =
  split_on ',' s
  |> List.map (fun p ->
         let a = parse_atom p in
         match a.dep_rel with
         | Some (Eq, v) -> (a.dep_name, Some v)
         | Some _ -> failwith ("non-'=' relation in Provides: " ^ p)
         | None -> (a.dep_name, None))

(* Stanza reader: "Key: value" fields with space/tab-prefixed continuations. *)
let parse_stanzas ic =
  let rec lines acc =
    match In_channel.input_line ic with
    | Some l -> lines (l :: acc)
    | None -> List.rev acc
  in
  (* fold lines into (finished stanzas, current fields, current field) *)
  let flush_field fields = function
    | Some (key, values) -> (key, String.concat " " (List.rev values)) :: fields
    | None -> fields
  in
  let flush_stanza stanzas fields field =
    match flush_field fields field with [] -> stanzas | fs -> List.rev fs :: stanzas
  in
  let step (stanzas, fields, field) line =
    if line = "" then (flush_stanza stanzas fields field, [], None)
    else if line.[0] = ' ' || line.[0] = '\t' then
      match field with
      | Some (key, values) -> (stanzas, fields, Some (key, strip line :: values))
      | None -> (stanzas, fields, None)
    else
      match String.index_opt line ':' with
      | Some i ->
          ( stanzas,
            flush_field fields field,
            Some
              ( String.sub line 0 i,
                [ strip (String.sub line (i + 1) (String.length line - i - 1)) ] ) )
      | None -> (stanzas, fields, field)
  in
  let stanzas, fields, field = List.fold_left step ([], [], None) (lines []) in
  List.rev (flush_stanza stanzas fields field)

let of_stanza fs =
  match List.assoc_opt "Package" fs with
  | None -> None
  | Some name ->
      let version_str = match List.assoc_opt "Version" fs with Some v -> v | None -> "" in
      let field k = match List.assoc_opt k fs with Some v -> v | None -> "" in
      let deps k = if field k = "" then [] else parse_dep_field (field k) in
      Some
        {
          name;
          version = Deb_version.parse version_str;
          version_str;
          depends = deps "Depends" @ deps "Pre-Depends";
          conflicts = List.concat (deps "Conflicts" @ deps "Breaks");
          provides = (if field "Provides" = "" then [] else parse_provides_field (field "Provides"));
        }

let parse_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> parse_stanzas ic |> List.filter_map of_stanza)
