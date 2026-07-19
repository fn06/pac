(* Semantic versions and Cargo requirement matching. *)

type pre_id = Num of int | Alnum of string

type t = { major : int; minor : int; patch : int; pre : pre_id list; orig : string }

let parse_pre s =
  if s = "" then []
  else
    String.split_on_char '.' s
    |> List.map (fun id ->
           match int_of_string_opt id with
           | Some n when id = string_of_int n -> Num n
           | _ -> Alnum id)

let parse s =
  let core, pre =
    match String.index_opt s '-' with
    | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
    | None -> (s, "")
  in
  (* strip build metadata from either part *)
  let strip_build x =
    match String.index_opt x '+' with Some i -> String.sub x 0 i | None -> x
  in
  let core = strip_build core and pre = strip_build pre in
  match String.split_on_char '.' core |> List.map int_of_string_opt with
  | [ Some m; Some n; Some p ] -> { major = m; minor = n; patch = p; pre = parse_pre pre; orig = s }
  | _ -> failwith ("bad semver: " ^ s)

let compare_pre a b =
  match (a, b) with
  | [], [] -> 0
  | [], _ -> 1 (* release > pre-release *)
  | _, [] -> -1
  | _ ->
      let rec go a b =
        match (a, b) with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | x :: a, y :: b -> (
            match (x, y) with
            | Num i, Num j -> if i <> j then Stdlib.compare i j else go a b
            | Num _, Alnum _ -> -1
            | Alnum _, Num _ -> 1
            | Alnum s, Alnum t -> if s <> t then Stdlib.compare s t else go a b)
      in
      go a b

let compare a b =
  let c = Stdlib.compare (a.major, a.minor, a.patch) (b.major, b.minor, b.patch) in
  if c <> 0 then c else compare_pre a.pre b.pre

let to_string v = v.orig

(* Granularity: the semver-compatibility class. *)
let granularity v =
  if v.major > 0 then string_of_int v.major
  else if v.minor > 0 then Printf.sprintf "0.%d" v.minor
  else Printf.sprintf "0.0.%d" v.patch

(* ---------- requirements ---------- *)

type partial = { p_major : int; p_minor : int option; p_patch : int option; p_pre : pre_id list }

type comparator =
  | Caret of partial
  | Tilde of partial
  | Exact of partial
  | GtC of partial
  | GeC of partial
  | LtC of partial
  | LeC of partial
  | Wildcard of partial option (* "*" or "1.*" / "1.2.*" *)

type req = comparator list (* conjunction *)

let parse_partial s =
  let core, pre =
    match String.index_opt s '-' with
    | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
    | None -> (s, "")
  in
  let parts = String.split_on_char '.' core in
  let wild x = x = "*" || x = "x" || x = "X" in
  match parts with
  | [ m ] when wild m -> None
  | m :: rest when not (wild m) ->
      let m = int_of_string m in
      let mi, pa =
        match rest with
        | [] -> (None, None)
        | [ n ] -> ((if wild n then None else Some (int_of_string n)), None)
        | [ n; p ] ->
            ( (if wild n then None else Some (int_of_string n)),
              if wild n || wild p then None else Some (int_of_string p) )
        | _ -> failwith ("bad requirement: " ^ s)
      in
      Some { p_major = m; p_minor = mi; p_patch = pa; p_pre = parse_pre pre }
  | _ -> failwith ("bad requirement: " ^ s)

let parse_comparator s =
  let s = String.trim s in
  let after prefix = String.trim (String.sub s (String.length prefix) (String.length s - String.length prefix)) in
  let part p = match parse_partial p with Some x -> x | None -> failwith ("bad requirement: " ^ s) in
  if s = "*" then Wildcard None
  else if String.starts_with ~prefix:"^" s then Caret (part (after "^"))
  else if String.starts_with ~prefix:"~" s then Tilde (part (after "~"))
  else if String.starts_with ~prefix:"=" s then Exact (part (after "="))
  else if String.starts_with ~prefix:">=" s then GeC (part (after ">="))
  else if String.starts_with ~prefix:"<=" s then LeC (part (after "<="))
  else if String.starts_with ~prefix:">" s then GtC (part (after ">"))
  else if String.starts_with ~prefix:"<" s then LtC (part (after "<"))
  else
    match parse_partial s with
    | None -> Wildcard None
    | Some p -> if String.contains s '*' || String.contains s 'x' then Wildcard (Some p) else Caret p

let parse_req s : req =
  String.split_on_char ',' s |> List.map String.trim |> List.filter (( <> ) "")
  |> List.map parse_comparator

let lower p = { major = p.p_major; minor = Option.value ~default:0 p.p_minor;
                patch = Option.value ~default:0 p.p_patch; pre = p.p_pre; orig = "" }

let matches_comparator v c =
  let ge_lower p = compare v (lower p) >= 0 in
  match c with
  | Exact p -> (
      match (p.p_minor, p.p_patch) with
      | Some n, Some q ->
          v.major = p.p_major && v.minor = n && v.patch = q && compare_pre v.pre p.p_pre = 0
      | Some n, None -> v.major = p.p_major && v.minor = n && v.pre = []
      | None, _ -> v.major = p.p_major && v.pre = [])
  | GeC p -> ge_lower p
  | GtC p -> (
      match (p.p_minor, p.p_patch) with
      | Some _, Some _ -> compare v (lower p) > 0
      | Some n, None -> Stdlib.compare (v.major, v.minor) (p.p_major, n) > 0
      | None, _ -> v.major > p.p_major)
  | LeC p -> (
      match (p.p_minor, p.p_patch) with
      | Some _, Some _ -> compare v (lower p) <= 0
      | Some n, None -> Stdlib.compare (v.major, v.minor) (p.p_major, n) <= 0
      | None, _ -> v.major <= p.p_major)
  | LtC p -> compare v (lower p) < 0
  | Wildcard None -> true
  | Wildcard (Some p) -> (
      match p.p_minor with
      | None -> v.major = p.p_major
      | Some n -> v.major = p.p_major && v.minor = n)
  | Caret p ->
      ge_lower p
      &&
      if p.p_major > 0 then v.major = p.p_major
      else (
        match p.p_minor with
        | None -> v.major = 0
        | Some 0 ->
            v.major = 0 && v.minor = 0
            && (match p.p_patch with None -> true | Some q -> v.patch = q)
        | Some n -> v.major = 0 && v.minor = n)
  | Tilde p -> (
      ge_lower p
      &&
      match p.p_minor with
      | None -> v.major = p.p_major
      | Some n -> v.major = p.p_major && v.minor = n)

(* Pre-release versions match only if some comparator names a pre-release of
   the same major.minor.patch. *)
let pre_ok v req =
  v.pre = []
  || List.exists
       (fun c ->
         let p =
           match c with
           | Caret p | Tilde p | Exact p | GtC p | GeC p | LtC p | LeC p -> Some p
           | Wildcard p -> p
         in
         match p with
         | Some p ->
             p.p_pre <> []
             && p.p_major = v.major
             && p.p_minor = Some v.minor
             && p.p_patch = Some v.patch
         | None -> false)
       req

let matches v (req : req) =
  pre_ok v req && List.for_all (matches_comparator v) req
