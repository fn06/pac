(* Lazy crates.io index access: crate files parse on first lookup. *)

type dep_kind = Normal | Build | Dev

type dep = {
  dep_name : string; (* registry crate (the `package` field when renamed) *)
  as_name : string; (* name used in feature specs *)
  req : Semver.req;
  req_str : string;
  dep_features : string list;
  optional : bool;
  default_features : bool;
  target : Opam_repo.filter option; (* cfg condition, as a filter tree *)
  kind : dep_kind;
}

type spec =
  | SFeature of string (* another feature of this crate *)
  | SDep of string (* dep:x — enable optional dep x (as-name) *)
  | SDepFeature of string * string * bool (* x/f, weak? (x?/f) *)

type entry = {
  version : Semver.t;
  vstr : string;
  deps : dep list;
  features : (string * spec list) list; (* includes implicit features *)
  yanked : bool;
  links : string option; (* native library claimed, at most one crate per graph *)
}

type t = { dir : string; memo : (string, entry list) Hashtbl.t }

let create dir = { dir; memo = Hashtbl.create 1024 }

let shard_path dir name =
  let name = String.lowercase_ascii name in
  let sub = String.sub in
  let p =
    match String.length name with
    | 1 -> Filename.concat "1" name
    | 2 -> Filename.concat "2" name
    | 3 -> Filename.concat (Filename.concat "3" (sub name 0 1)) name
    | _ -> Filename.concat (Filename.concat (sub name 0 2) (sub name 2 2)) name
  in
  Filename.concat dir p

(* ---------- cfg parsing ---------- *)

(* cfg(all(unix, not(windows))), cfg(target_os = "macos"), or a bare triple.
   Predicates become variable comparisons in the shared filter language. *)
let parse_cfg s : Opam_repo.filter =
  let open Opam_repo in
  let n = String.length s in
  let rec skip_ws i = if i < n && (s.[i] = ' ' || s.[i] = '\t') then skip_ws (i + 1) else i in
  let ident_char = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false in
  let rec span p i = if i < n && p s.[i] then span p (i + 1) else i in
  let ident i = let j = span ident_char i in (String.sub s i (j - i), j) in
  let expect c i =
    let i = skip_ws i in
    if i < n && s.[i] = c then i + 1 else failwith ("cfg parse error in: " ^ s)
  in
  let string_lit i =
    let i = expect '"' i in
    let j = span (( <> ) '"') i in
    (String.sub s i (j - i), expect '"' j)
  in
  let rec pred i =
    let id, i = ident (skip_ws i) in
    let i = skip_ws i in
    match id with
    | "all" | "any" ->
        let args, i = args [] (expect '(' i) in
        let op a b = if id = "all" then FAnd (a, b) else FOr (a, b) in
        let f =
          match args with [] -> FBool (id = "all") | x :: rest -> List.fold_left op x rest
        in
        (f, expect ')' i)
    | "not" ->
        let x, i = pred (expect '(' i) in
        (FNot x, expect ')' i)
    | "unix" | "windows" -> (FCmp ("target_family", Eq, id), i)
    | _ ->
        if i < n && s.[i] = '=' then
          let v, i = string_lit (skip_ws (i + 1)) in
          (FCmp (id, Eq, v), i)
        else (FVar id, i)
  and args acc i =
    let i = skip_ws i in
    if i < n && s.[i] = ')' then (List.rev acc, i)
    else
      let x, i = pred i in
      let i = skip_ws i in
      if i < n && s.[i] = ',' then args (x :: acc) (i + 1) else (List.rev (x :: acc), i)
  in
  if String.starts_with ~prefix:"cfg(" s then fst (pred 4)
  else Opam_repo.FCmp ("target", Eq, s) (* a literal target triple *)

(* ---------- JSON parsing ---------- *)

let parse_spec s : spec =
  match String.index_opt s '/' with
  | Some i ->
      let d = String.sub s 0 i and f = String.sub s (i + 1) (String.length s - i - 1) in
      if String.length d > 0 && d.[String.length d - 1] = '?' then
        SDepFeature (String.sub d 0 (String.length d - 1), f, true)
      else SDepFeature (d, f, false)
  | None ->
      if String.starts_with ~prefix:"dep:" s then
        SDep (String.sub s 4 (String.length s - 4))
      else SFeature s

let parse_entry line =
  let open Yojson.Safe.Util in
  let j = Yojson.Safe.from_string line in
  let vstr = j |> member "vers" |> to_string in
  let deps =
    j |> member "deps" |> to_list
    |> List.filter_map (fun d ->
           let as_name = d |> member "name" |> to_string in
           let dep_name =
             match d |> member "package" with `String p -> p | _ -> as_name
           in
           let kind =
             match d |> member "kind" with
             | `String "build" -> Build
             | `String "dev" -> Dev
             | _ -> Normal
           in
           let req_str = d |> member "req" |> to_string in
           match Semver.parse_req req_str with
           | req ->
               Some
                 {
                   dep_name;
                   as_name;
                   req;
                   req_str;
                   dep_features =
                     (match d |> member "features" with
                     | `List l -> List.map to_string l
                     | _ -> []);
                   optional = (match d |> member "optional" with `Bool b -> b | _ -> false);
                   default_features =
                     (match d |> member "default_features" with `Bool b -> b | _ -> true);
                   target =
                     (match d |> member "target" with
                     | `String t -> ( try Some (parse_cfg t) with _ -> Some (Opam_repo.FBool false))
                     | _ -> None);
                   kind;
                 }
           | exception _ -> None)
  in
  let feature_field name =
    match j |> member name with
    | `Assoc l ->
        List.map
          (fun (f, specs) ->
            (f, (match specs with `List l -> List.map to_string l | _ -> []) |> List.map parse_spec))
          l
    | _ -> []
  in
  let features = feature_field "features" @ feature_field "features2" in
  (* implicit features: an optional dep with no dep:x mention anywhere gets an
     implicit feature of its as-name *)
  let mentioned =
    List.concat_map (fun (_, specs) ->
        List.filter_map (function SDep x -> Some x | _ -> None) specs)
      features
  in
  let implicit =
    deps
    |> List.filter (fun d -> d.optional)
    |> List.filter (fun d -> not (List.mem d.as_name mentioned))
    |> List.filter (fun d -> not (List.mem_assoc d.as_name features))
    |> List.map (fun d -> (d.as_name, [ SDep d.as_name ]))
  in
  {
    version = Semver.parse vstr;
    vstr;
    deps;
    features = features @ implicit;
    yanked = (match j |> member "yanked" with `Bool b -> b | _ -> false);
    links = (match j |> member "links" with `String s -> Some s | _ -> None);
  }

let entries t name =
  match Hashtbl.find_opt t.memo name with
  | Some es -> es
  | None ->
      let path = shard_path t.dir name in
      let es =
        if Sys.file_exists path then
          In_channel.with_open_text path (fun ic ->
              let rec lines acc =
                match In_channel.input_line ic with
                | Some l -> lines (l :: acc)
                | None -> List.rev acc
              in
              lines [])
          |> List.filter (( <> ) "")
          |> List.filter_map (fun line ->
                 try Some (parse_entry line)
                 with _ ->
                   Printf.eprintf "warning: bad index entry for %s\n%!" name;
                   None)
        else []
      in
      let es =
        List.filter (fun e -> not e.yanked) es
        |> List.sort (fun a b -> Semver.compare b.version a.version)
      in
      Hashtbl.replace t.memo name es;
      es

let crates_parsed t = Hashtbl.length t.memo
