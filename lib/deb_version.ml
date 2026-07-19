(* dpkg version comparison per deb-version(7): [epoch:]upstream[-revision]. *)

type t = { epoch : int; upstream : string; revision : string }

let parse s =
  let epoch, rest =
    match String.index_opt s ':' with
    | Some i -> (
        match int_of_string_opt (String.sub s 0 i) with
        | Some e -> (e, String.sub s (i + 1) (String.length s - i - 1))
        | None -> (0, s))
    | None -> (0, s)
  in
  let upstream, revision =
    match String.rindex_opt rest '-' with
    | Some i ->
        (String.sub rest 0 i, String.sub rest (i + 1) (String.length rest - i - 1))
    | None -> (rest, "")
  in
  { epoch; upstream; revision }

let digit c = c >= '0' && c <= '9'
let alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

(* '~' sorts before everything, including the end of the string. *)
let order c =
  if digit c then 0
  else if alpha c then Char.code c
  else if c = '~' then -1
  else Char.code c + 256

let verrevcmp a b =
  let la = String.length a and lb = String.length b in
  let order_at s l i = if i < l then order s.[i] else 0 in
  let digit_at s l i = i < l && digit s.[i] in
  let skip p s l i = let rec go i = if i < l && p s.[i] then go (i + 1) else i in go i in
  (* alternate phases: compare non-digit runs by character order, then digit
     runs numerically (leading zeros stripped; longer run wins, ties lexical) *)
  let rec nondigits i j =
    if (i < la && not (digit_at a la i)) || (j < lb && not (digit_at b lb j)) then
      let c = Stdlib.compare (order_at a la i) (order_at b lb j) in
      if c <> 0 then c else nondigits (i + 1) (j + 1)
    else digits i j
  and digits i j =
    let i = skip (( = ) '0') a la i and j = skip (( = ) '0') b lb j in
    let i' = skip digit a la i and j' = skip digit b lb j in
    let c = Stdlib.compare (i' - i) (j' - j) in
    if c <> 0 then c
    else
      let c = Stdlib.compare (String.sub a i (i' - i)) (String.sub b j (j' - j)) in
      if c <> 0 then c
      else if i' >= la && j' >= lb then 0
      else nondigits i' j'
  in
  nondigits 0 0

let compare v1 v2 =
  let c = Stdlib.compare v1.epoch v2.epoch in
  if c <> 0 then c
  else
    let c = verrevcmp v1.upstream v2.upstream in
    if c <> 0 then c else verrevcmp v1.revision v2.revision

let to_string v =
  (if v.epoch = 0 then "" else string_of_int v.epoch ^ ":")
  ^ v.upstream
  ^ if v.revision = "" then "" else "-" ^ v.revision

let pp fmt v = Format.pp_print_string fmt (to_string v)
