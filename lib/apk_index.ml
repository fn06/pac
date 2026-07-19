(* APKINDEX parser: single-letter fields, blank-line-separated stanzas. *)

type atom = {
  a_name : string; (* may be namespaced: so:…, cmd:…, pc:… *)
  a_rel : (Apk_version.rel * Apk_version.t) option;
  a_rel_str : string; (* canonical, e.g. ">=1.24"; "" if none *)
  a_neg : bool; (* !name conflict *)
}

type pkg = {
  name : string;
  version : Apk_version.t;
  version_str : string;
  depends : atom list; (* positive atoms *)
  conflicts : atom list; (* the !-atoms *)
  provides : (string * Apk_version.t option) list;
  priority : int; (* k: provider priority, 0 if absent *)
  install_if : atom list; (* i: auto-install when all are satisfied *)
}

let parse_atom s =
  let neg = String.length s > 0 && s.[0] = '!' in
  let s = if neg then String.sub s 1 (String.length s - 1) else s in
  let oplen c c2 =
    match (c, c2) with
    | '<', Some '=' | '>', Some '=' -> 2
    | ('<' | '>' | '=' | '~'), _ -> 1
    | _ -> 0
  in
  let n = String.length s in
  let rec find i =
    if i >= n then None
    else
      match oplen s.[i] (if i + 1 < n then Some s.[i + 1] else None) with
      | 0 -> find (i + 1)
      | l -> Some (i, l)
  in
  match find 0 with
  | None -> { a_name = s; a_rel = None; a_rel_str = ""; a_neg = neg }
  | Some (i, l) ->
      let name = String.sub s 0 i in
      let op = String.sub s i l in
      let ver = String.sub s (i + l) (n - i - l) in
      let rel : Apk_version.rel =
        match op with
        | "<" -> Lt
        | "<=" -> Le
        | "=" -> Eq
        | ">=" -> Ge
        | ">" -> Gt
        | "~" -> Fuzzy
        | _ -> Eq
      in
      {
        a_name = name;
        a_rel = Some (rel, Apk_version.parse ver);
        a_rel_str = op ^ ver;
        a_neg = neg;
      }

let parse_provide s =
  let a = parse_atom s in
  match a.a_rel with
  | Some (Apk_version.Eq, v) -> (a.a_name, Some v)
  | _ -> (a.a_name, None)

type t = {
  by_name : (string, pkg list) Hashtbl.t; (* newest first *)
  by_id : (string * string, pkg) Hashtbl.t;
  providers : (string, pkg * Apk_version.t option) Hashtbl.t;
  conflicts_against : (string, pkg * atom) Hashtbl.t;
  install_if_against : (string, pkg * int * atom) Hashtbl.t; (* atom target -> rule *)
  rules : pkg list; (* packages with install_if, for the global trigger edges *)
}

let parse_file path =
  let stanza_of fields =
    match (List.assoc_opt 'P' fields, List.assoc_opt 'V' fields) with
    | Some name, Some vstr ->
        let split v = String.split_on_char ' ' v |> List.filter (( <> ) "") in
        let atoms =
          match List.assoc_opt 'D' fields with
          | Some d -> List.map parse_atom (split d)
          | None -> []
        in
        let deps, confl = List.partition (fun a -> not a.a_neg) atoms in
        Some
          {
            name;
            version = Apk_version.parse vstr;
            version_str = vstr;
            depends = deps;
            conflicts = confl;
            provides =
              (match List.assoc_opt 'p' fields with
              | Some p -> List.map parse_provide (split p)
              | None -> []);
            priority =
              (match List.assoc_opt 'k' fields with
              | Some k -> Option.value ~default:0 (int_of_string_opt k)
              | None -> 0);
            install_if =
              (match List.assoc_opt 'i' fields with
              | Some i -> List.map parse_atom (split i)
              | None -> []);
          }
    | _ -> None
  in
  In_channel.with_open_text path (fun ic ->
      let rec lines acc =
        match In_channel.input_line ic with
        | Some l -> lines (l :: acc)
        | None -> List.rev acc
      in
      let step (pkgs, fields) line =
        if line = "" then
          ((match stanza_of (List.rev fields) with Some p -> p :: pkgs | None -> pkgs), [])
        else if String.length line > 2 && line.[1] = ':' then
          (pkgs, (line.[0], String.sub line 2 (String.length line - 2)) :: fields)
        else (pkgs, fields)
      in
      let pkgs, fields = List.fold_left step ([], []) (lines []) in
      let pkgs =
        match stanza_of (List.rev fields) with Some p -> p :: pkgs | None -> pkgs
      in
      List.rev pkgs)

let load path =
  let pkgs = parse_file path in
  let t =
    {
      by_name = Hashtbl.create 4096;
      by_id = Hashtbl.create 8192;
      providers = Hashtbl.create 4096;
      conflicts_against = Hashtbl.create 512;
      install_if_against = Hashtbl.create 2048;
      rules = List.filter (fun p -> p.install_if <> []) pkgs;
    }
  in
  List.iter
    (fun p ->
      if not (Hashtbl.mem t.by_id (p.name, p.version_str)) then begin
        Hashtbl.replace t.by_id (p.name, p.version_str) p;
        Hashtbl.replace t.by_name p.name
          (p :: Option.value ~default:[] (Hashtbl.find_opt t.by_name p.name));
        List.iter (fun (n, v) -> Hashtbl.add t.providers n (p, v)) p.provides;
        List.iter (fun a -> Hashtbl.add t.conflicts_against a.a_name (p, a)) p.conflicts;
        List.iteri (fun i a -> Hashtbl.add t.install_if_against a.a_name (p, i, a)) p.install_if
      end)
    pkgs;
  Hashtbl.filter_map_inplace
    (fun _ ps ->
      Some (List.sort (fun a b -> Apk_version.compare b.version a.version) ps))
    t.by_name;
  t
