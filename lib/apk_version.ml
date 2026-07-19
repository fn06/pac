(* apk version ordering per apk-tools' version.c: dotted components (leading
   zeros compare as strings), optional trailing letter, suffix chain with
   pre/post ranks, -rN revision. *)

type suffix = { s_rank : int; s_num : int }

type t = {
  parts : string list; (* dotted numeric components, kept as strings *)
  letter : char option;
  suffixes : suffix list;
  rev : int;
  orig : string;
}

let suffix_rank = function
  | "alpha" -> 0
  | "beta" -> 1
  | "pre" -> 2
  | "rc" -> 3
  | "cvs" -> 5
  | "svn" -> 6
  | "git" -> 7
  | "hg" -> 8
  | "p" -> 9
  | _ -> 4 (* unknown: treat as release *)

let none_rank = 4

let parse s =
  let n = String.length s in
  let digit c = c >= '0' && c <= '9' in
  let alpha c = c >= 'a' && c <= 'z' in
  let rec span p i = if i < n && p s.[i] then span p (i + 1) else i in
  let take p i = (String.sub s i (span p i - i), span p i) in
  let digits = take digit and alphas = take alpha in
  let rec parts acc i =
    let d, i = digits i in
    let acc = d :: acc in
    if i + 1 < n && s.[i] = '.' && digit s.[i + 1] then parts acc (i + 1)
    else (List.rev acc, i)
  in
  let letter i =
    if i < n && alpha s.[i] && not (i + 1 < n && alpha s.[i + 1]) then (Some s.[i], i + 1)
    else (None, i)
  in
  let rec suffixes acc i =
    if i < n && s.[i] = '_' then
      let name, i = alphas (i + 1) in
      let num, i = digits i in
      suffixes
        ({ s_rank = suffix_rank name; s_num = (if num = "" then 0 else int_of_string num) }
        :: acc)
        i
    else (List.rev acc, i)
  in
  let rev i =
    if i + 1 < n && s.[i] = '-' && s.[i + 1] = 'r' then
      match digits (i + 2) with "", _ -> 0 | d, _ -> int_of_string d
    else 0
  in
  let parts, i = parts [] 0 in
  let letter, i = letter i in
  let suffixes, i = suffixes [] i in
  { parts; letter; suffixes; rev = rev i; orig = s }

let compare_part a b =
  (* leading zeros: compare as strings (fractional); else numerically *)
  if (String.length a > 1 && a.[0] = '0') || (String.length b > 1 && b.[0] = '0') then
    Stdlib.compare a b
  else
    match (int_of_string_opt a, int_of_string_opt b) with
    | Some x, Some y -> Stdlib.compare x y
    | _ -> Stdlib.compare a b

let rec compare_parts a b =
  match (a, b) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: a, y :: b ->
      let c = compare_part x y in
      if c <> 0 then c else compare_parts a b

let rec compare_suffixes a b =
  match (a, b) with
  | [], [] -> 0
  | [], s :: _ -> Stdlib.compare none_rank s.s_rank
  | s :: _, [] -> Stdlib.compare s.s_rank none_rank
  | x :: a, y :: b ->
      let c = Stdlib.compare (x.s_rank, x.s_num) (y.s_rank, y.s_num) in
      if c <> 0 then c else compare_suffixes a b

let compare a b =
  let c = compare_parts a.parts b.parts in
  if c <> 0 then c
  else
    let c = Stdlib.compare a.letter b.letter in
    if c <> 0 then c
    else
      let c = compare_suffixes a.suffixes b.suffixes in
      if c <> 0 then c else Stdlib.compare a.rev b.rev

let to_string v = v.orig

type rel = Lt | Le | Eq | Ge | Gt | Fuzzy

let satisfies v = function
  | None -> true
  | Some (rel, w) -> (
      match rel with
      | Fuzzy ->
          (* prefix match on the dotted components *)
          let rec prefix a b =
            match (a, b) with
            | [], _ -> true
            | x :: a, y :: b -> compare_part x y = 0 && prefix a b
            | _, [] -> false
          in
          prefix w.parts v.parts
      | _ -> (
          let c = compare v w in
          match rel with
          | Lt -> c < 0
          | Le -> c <= 0
          | Eq -> c = 0
          | Ge -> c >= 0
          | Gt -> c > 0
          | Fuzzy -> assert false))
