(* Cargo frontend: the Concurrent Feature composition as lazy lookups.
   Granular packages per semver-compatibility class, feature packages whose
   versions are the support relation, split intermediates whose version
   selects the dependee's class, and class-agreement selectors tying
   feature-spec edges to the same class choice. cfg conditions go through the
   variable machinery. *)

open Cargo_index

type config = {
  index : Cargo_index.t;
  sigma : (string * string) list;
  free : string list;
  links_seen : (string, (string * string) list) Hashtbl.t;
      (* links string -> claimant (crate, class) pairs visited so far *)
}

let default_sigma =
  [
    ("target_family", "unix");
    ("target_os", "linux");
    ("target_arch", "x86_64");
    ("target_env", "gnu");
    ("target_endian", "little");
    ("target_pointer_width", "64");
    ("target", "x86_64-unknown-linux-gnu");
  ]

let undefined = "%undefined%"

let sigma_get cfg x =
  match List.assoc_opt x cfg.sigma with Some v -> v | None -> undefined

(* ---------- solver types ---------- *)

module Name = struct
  type t =
    | G of string * string (* crate, compatibility class *)
    | F of string * string * string (* crate, class, feature *)
    | I of string * string * string (* depender crate+version, dep as-name *)
    | Sp of string * string * string * string (* … plus a feature of the dependee *)
    | C of string * string * int (* cfg alt: depender crate+version, dep index *)
    | L of string (* links conflict class: the native library string *)
    | D of string * string (* dev shim for a queried crate: crate, class *)
    | Var of string

  let compare = Stdlib.compare

  let pp fmt = function
    | G (n, w) -> Format.fprintf fmt "%s@%s" n w
    | F (n, w, f) -> Format.fprintf fmt "%s@%s+%s" n w f
    | I (n, v, m) -> Format.fprintf fmt "<%s.%s->%s>" n v m
    | Sp (n, v, m, f) -> Format.fprintf fmt "<%s.%s->%s+%s>" n v m f
    | C (n, v, k) -> Format.fprintf fmt "<%s.%s cfg%d>" n v k
    | L s -> Format.fprintf fmt "<links:%s>" s
    | D (n, w) -> Format.fprintf fmt "<dev:%s@%s>" n w
    | Var x -> Format.fprintf fmt "<%s>" x
end

module Ver = struct
  type t = B of int | Val of string | V of Semver.t * string

  let rank = function B _ -> 0 | Val _ -> 1 | V _ -> 2

  let compare a b =
    match (a, b) with
    | V (x, _), V (y, _) -> Semver.compare x y
    | Val x, Val y -> Stdlib.compare x y
    | B i, B j -> Stdlib.compare j i (* first branch preferred *)
    | _ -> Stdlib.compare (rank a) (rank b)

  let pp fmt = function
    | V (_, s) | Val s -> Format.pp_print_string fmt s
    | B i -> Format.pp_print_int fmt i
end

module Solver = Pubgrub.Make (Name) (Ver)

let of_list = Solver.Ranges.of_list

(* ---------- index helpers ---------- *)

let matching cfg name req =
  entries cfg.index name |> List.filter (fun e -> Semver.matches e.version req)

(* compatibility classes of the matching versions, newest class first *)
let classes_of cfg name req =
  let es = matching cfg name req in
  let ws =
    List.map (fun e -> Semver.granularity e.version) es |> List.sort_uniq Stdlib.compare
  in
  let maxv w =
    List.filter (fun e -> Semver.granularity e.version = w) es
    |> List.fold_left
         (fun acc e -> if Semver.compare e.version acc > 0 then e.version else acc)
         (Semver.parse "0.0.0-0")
  in
  List.sort (fun w1 w2 -> Semver.compare (maxv w2) (maxv w1)) ws

let class_versions cfg name req w =
  matching cfg name req |> List.filter (fun e -> Semver.granularity e.version = w)

let entry cfg name vstr =
  entries cfg.index name |> List.find_opt (fun e -> e.vstr = vstr)

(* Claimants of a links string are discovered by the solver's own traversal;
   an id per (crate, class) makes version uniqueness the n-way exclusion. *)
let links_id (n, w) = n ^ "@" ^ w

let links_edges cfg (n, w) e =
  match e.links with
  | None -> []
  | Some s ->
      let cur = Option.value ~default:[] (Hashtbl.find_opt cfg.links_seen s) in
      if not (List.mem (n, w) cur) then Hashtbl.replace cfg.links_seen s ((n, w) :: cur);
      [ (Name.L s, of_list [ Ver.Val (links_id (n, w)) ]) ]

let has_feature e f = f = "default" || List.mem_assoc f e.features

(* ---------- variable conditions (cfg) ---------- *)

let domain cfg x =
  if List.mem x cfg.free then
    match x with
    | "target_os" -> [ "linux"; "macos"; "windows"; "freebsd"; "android"; "ios"; "wasi" ]
    | "target_family" -> [ "unix"; "windows"; "wasm" ]
    | "target_arch" -> [ "x86_64"; "aarch64"; "x86"; "arm"; "wasm32" ]
    | "target_env" -> [ "gnu"; "musl"; "msvc" ]
    | _ -> [ sigma_get cfg x ]
  else [ sigma_get cfg x ]

let holds rel u y =
  (* an unset variable differs from every value *)
  if u = undefined then rel = Opam_repo.Neq
  else Opam_repo.rel_holds rel (Stdlib.compare u y)

let var_edge cfg x rel y =
  let dom = domain cfg x in
  ( Name.Var x,
    of_list (List.filter_map (fun u -> if holds rel u y then Some (Ver.Val u) else None) dom)
  )

let cfg_literals f =
  let add acc l = if List.mem l acc then acc else l :: acc in
  let rec go acc = function
    | Opam_repo.FBool _ | Opam_repo.FVer _ -> acc
    | Opam_repo.FVar x -> add acc (x, Opam_repo.Eq, "true")
    | Opam_repo.FCmp (x, r, y) -> add acc (x, r, y)
    | Opam_repo.FNot g -> go acc g
    | Opam_repo.FAnd (a, b) | Opam_repo.FOr (a, b) -> go (go acc a) b
  in
  List.rev (go [] f)

let rec cfg_eval alpha = function
  | Opam_repo.FBool b -> b
  | Opam_repo.FVer _ -> true
  | Opam_repo.FVar x -> List.assoc (x, Opam_repo.Eq, "true") alpha
  | Opam_repo.FCmp (x, r, y) -> List.assoc (x, r, y) alpha
  | Opam_repo.FNot f -> not (cfg_eval alpha f)
  | Opam_repo.FAnd (a, b) -> cfg_eval alpha a && cfg_eval alpha b
  | Opam_repo.FOr (a, b) -> cfg_eval alpha a || cfg_eval alpha b

let rec assignments = function
  | [] -> [ [] ]
  | l :: rest ->
      let tails = assignments rest in
      List.map (fun t -> (l, true) :: t) tails @ List.map (fun t -> (l, false) :: t) tails

(* ---------- dependency encoding ---------- *)

let direct_edges cfg name req w feats =
  let vs e = Ver.V (e.version, e.vstr) in
  let base = class_versions cfg name req w in
  (Name.G (name, w), of_list (List.map vs base))
  :: List.map
       (fun f ->
         if f = "default" then
           (* versions lacking a default feature are fine bare *)
           (Name.F (name, w, "default"), of_list (List.map vs base))
         else
           ( Name.F (name, w, f),
             of_list (List.map vs (List.filter (fun e -> has_feature e f) base)) ))
       feats

(* Edges for dependency [d] of package (n, v), ignoring its cfg condition. *)
let dep_edges cfg (n, v) (d : dep) : (Name.t * Solver.Ranges.t) list =
  let feats = (if d.default_features then [ "default" ] else []) @ d.dep_features in
  match classes_of cfg d.dep_name d.req with
  | [] -> [ (Name.G (d.dep_name, "?"), of_list []) ] (* unsatisfiable *)
  | [ w ] -> direct_edges cfg d.dep_name d.req w feats
  | ws -> [ (Name.I (n, v, d.as_name), of_list (List.mapi (fun j _ -> Ver.B j) ws)) ]

let find_dep e as_name = List.find_opt (fun d -> d.as_name = as_name) e.deps

(* Edges requiring feature [f] of the dependee of [d], agreeing on the class
   choice with the main intermediate when the dependency is split. *)
let spec_feature_edges cfg (n, v) (d : dep) f =
  match classes_of cfg d.dep_name d.req with
  | [] -> [ (Name.G (d.dep_name, "?"), of_list []) ]
  | [ w ] ->
      let base = class_versions cfg d.dep_name d.req w in
      [ ( Name.F (d.dep_name, w, f),
          of_list
            (List.map
               (fun e -> Ver.V (e.version, e.vstr))
               (List.filter (fun e -> has_feature e f) base)) ) ]
  | ws ->
      ignore ws;
      [ (Name.Sp (n, v, d.as_name, f), of_list (List.mapi (fun j _ -> Ver.B j) ws)) ]

let with_cfg (n, v) k (d : dep) edges =
  match d.target with
  | None -> edges ()
  | Some f ->
      [ ( Name.C (n, v, k),
          of_list (List.mapi (fun j _ -> Ver.B j) (assignments (cfg_literals f))) ) ]

let cfg_branch_edges cfg f j edges =
  let lits = cfg_literals f in
  match List.nth_opt (assignments lits) j with
  | None -> []
  | Some alpha ->
      let cond =
        List.map
          (fun ((x, r, y), b) ->
            if b then var_edge cfg x r y else var_edge cfg x (Opam_repo.complement r) y)
          alpha
      in
      if cfg_eval alpha f then cond @ edges () else cond

(* ---------- the lookups ---------- *)

let dependencies cfg (name : Name.t) (ver : Ver.t) : (Name.t * Solver.Ranges.t) list =
  match (name, ver) with
  | G (n, w), V (_, vstr) -> (
      match entry cfg n vstr with
      | None -> []
      | Some e ->
          links_edges cfg (n, w) e
          @ (e.deps
            |> List.mapi (fun k d -> (k, d))
            |> List.filter (fun (_, d) -> d.kind <> Dev && not d.optional)
            |> List.concat_map (fun (k, d) ->
                   with_cfg (n, vstr) k d (fun () -> dep_edges cfg (n, vstr) d))))
  | F (n, w, f), V (_, vstr) -> (
      match entry cfg n vstr with
      | None -> []
      | Some e ->
          let base = [ (Name.G (n, w), of_list [ Ver.V (e.version, e.vstr) ]) ] in
          let specs = match List.assoc_opt f e.features with Some s -> s | None -> [] in
          base
          @ List.concat_map
              (function
                | SFeature g ->
                    if List.mem_assoc g e.features || g = "default" then
                      [ (Name.F (n, w, g), of_list [ Ver.V (e.version, e.vstr) ]) ]
                    else (
                      (* legacy: bare name of an optional dep enables it *)
                      match find_dep e g with
                      | Some d -> dep_edges cfg (n, vstr) d
                      | None -> [])
                | SDep x -> (
                    match find_dep e x with
                    | Some d -> dep_edges cfg (n, vstr) d
                    | None -> [])
                | SDepFeature (_, _, true) -> [] (* weak: v1 no-op *)
                | SDepFeature (x, f2, false) -> (
                    match find_dep e x with
                    | Some d ->
                        (if d.optional then dep_edges cfg (n, vstr) d else [])
                        @ spec_feature_edges cfg (n, vstr) d f2
                    | None -> []))
              specs)
  | I (n, v, as_name), B j -> (
      match entry cfg n v with
      | None -> []
      | Some e -> (
          match find_dep e as_name with
          | None -> []
          | Some d -> (
              let ws = classes_of cfg d.dep_name d.req in
              match List.nth_opt ws j with
              | None -> []
              | Some w ->
                  let feats =
                    (if d.default_features then [ "default" ] else []) @ d.dep_features
                  in
                  direct_edges cfg d.dep_name d.req w feats)))
  | Sp (n, v, as_name, f), B j -> (
      match entry cfg n v with
      | None -> []
      | Some e -> (
          match find_dep e as_name with
          | None -> []
          | Some d -> (
              let ws = classes_of cfg d.dep_name d.req in
              match List.nth_opt ws j with
              | None -> []
              | Some w ->
                  let base = class_versions cfg d.dep_name d.req w in
                  [ ( Name.F (d.dep_name, w, f),
                      of_list
                        (List.map
                           (fun e -> Ver.V (e.version, e.vstr))
                           (List.filter (fun e -> has_feature e f) base)) );
                    (* agree with the main intermediate's class choice *)
                    (Name.I (n, v, as_name), of_list [ Ver.B j ]) ])))
  | C (n, v, k), B j -> (
      match entry cfg n v with
      | None -> []
      | Some e -> (
          match List.nth_opt e.deps k with
          | None -> []
          | Some d -> (
              match d.target with
              | None -> []
              | Some f -> cfg_branch_edges cfg f j (fun () -> dep_edges cfg (n, v) d))))
  | D (n, w), V (_, vstr) -> (
      (* dev-dependencies of a queried crate; never transitive, so they hang
         off this root-only shim, agreeing with the class's version choice *)
      match entry cfg n vstr with
      | None -> []
      | Some e ->
          (Name.G (n, w), of_list [ Ver.V (e.version, e.vstr) ])
          :: (e.deps
             |> List.mapi (fun k d -> (k, d))
             |> List.filter (fun (_, d) -> d.kind = Dev)
             |> List.concat_map (fun (k, d) ->
                    with_cfg (n, vstr) k d (fun () -> dep_edges cfg (n, vstr) d))))
  | _ -> []

let versions cfg (name : Name.t) : Ver.t list =
  match name with
  | G (n, w) ->
      entries cfg.index n
      |> List.filter (fun e -> Semver.granularity e.version = w)
      |> List.map (fun e -> Ver.V (e.version, e.vstr))
  | F (n, w, f) ->
      entries cfg.index n
      |> List.filter (fun e -> Semver.granularity e.version = w)
      |> List.filter (fun e -> has_feature e f)
      |> List.map (fun e -> Ver.V (e.version, e.vstr))
  | I (n, v, as_name) | Sp (n, v, as_name, _) -> (
      match entry cfg n v with
      | None -> []
      | Some e -> (
          match find_dep e as_name with
          | None -> []
          | Some d -> List.mapi (fun j _ -> Ver.B j) (classes_of cfg d.dep_name d.req)))
  | C (n, v, k) -> (
      match entry cfg n v with
      | None -> []
      | Some e -> (
          match List.nth_opt e.deps k with
          | None -> []
          | Some d -> (
              match d.target with
              | None -> []
              | Some f -> List.mapi (fun j _ -> Ver.B j) (assignments (cfg_literals f)))))
  | L s ->
      Hashtbl.find_opt cfg.links_seen s
      |> Option.value ~default:[]
      |> List.map (fun c -> Ver.Val (links_id c))
  | D (n, w) ->
      entries cfg.index n
      |> List.filter (fun e -> Semver.granularity e.version = w)
      |> List.map (fun e -> Ver.V (e.version, e.vstr))
  | Var x -> List.map (fun u -> Ver.Val u) (domain cfg x)

(* ---------- query, decode, check ---------- *)

type query_atom = { q_name : string; q_req : Semver.req; q_feats : string list }

let solve ?(dev = false) cfg (query : query_atom list) =
  (* a query atom takes the newest matching compatibility class, like cargo add *)
  let edges =
    List.concat_map
      (fun a ->
        match classes_of cfg a.q_name a.q_req with
        | [] -> [ (Name.G (a.q_name, "?"), of_list []) ]
        | w :: _ ->
            direct_edges cfg a.q_name a.q_req w ("default" :: a.q_feats)
            @
            if dev then
              [ ( Name.D (a.q_name, w),
                  of_list
                    (List.map
                       (fun e -> Ver.V (e.version, e.vstr))
                       (class_versions cfg a.q_name a.q_req w)) ) ]
            else [])
      query
  in
  Solver.solve ~versions:(versions cfg) ~dependencies:(dependencies cfg) edges

let decode cfg solution =
  let pkgs =
    List.filter_map
      (function Name.G (n, _), Ver.V (_, s) -> Some (n, s) | _ -> None)
      solution
    |> List.sort Stdlib.compare
  in
  let feats =
    List.filter_map
      (function
        | Name.F (n, _, f), Ver.V (_, s) when f <> "default" -> Some ((n, s), f)
        | _ -> None)
      solution
  in
  let feats_of p =
    List.filter_map (fun (q, f) -> if q = p then Some f else None) feats
    |> List.sort_uniq Stdlib.compare
  in
  let free_choices =
    List.filter_map
      (function
        | Name.Var x, Ver.Val v when List.mem x cfg.free -> Some (x, v) | _ -> None)
      solution
    |> List.sort Stdlib.compare
  in
  (pkgs, feats_of, free_choices)

(* Best-effort validation against cargo semantics. *)
let check ?(dev_roots = []) cfg (pkgs : (string * string) list) feats_of free_choices =
  let cfg = { cfg with sigma = free_choices @ cfg.sigma; free = [] } in
  let keys = List.map (fun (n, v) -> (n, Semver.granularity (Semver.parse v))) pkgs in
  let uniqueness =
    if List.length (List.sort_uniq Stdlib.compare keys) <> List.length keys then
      [ "one version per compatibility class violated" ]
    else []
  in
  let links_errors =
    let claims =
      List.filter_map
        (fun (n, v) ->
          match entry cfg n v with
          | Some e -> Option.map (fun s -> (s, (n, v))) e.links
          | None -> None)
        pkgs
    in
    List.map fst claims
    |> List.sort_uniq Stdlib.compare
    |> List.filter_map (fun s ->
           match List.filter (fun (s', _) -> s' = s) claims with
           | _ :: _ :: _ as cs ->
               Some
                 (Printf.sprintf "native library %s linked by multiple crates: %s" s
                    (String.concat ", " (List.map (fun (_, (n, v)) -> n ^ " " ^ v) cs)))
           | _ -> None)
  in
  let selected m req =
    List.filter (fun (n, v) -> n = m && Semver.matches (Semver.parse v) req) pkgs
  in
  let cfg_true f =
    let lits = cfg_literals f in
    let alpha = List.map (fun ((x, r, y) as l) -> (l, holds r (sigma_get cfg x) y)) lits in
    cfg_eval alpha f
  in
  let cfg_applies d = match d.target with None -> true | Some f -> cfg_true f in
  let dep_applies d = d.kind <> Dev && cfg_applies d in
  let dep_errors ?(applies = dep_applies) (n, v) (d : dep) =
    if not (applies d) then []
    else
      match selected d.dep_name d.req with
      | [] -> [ Printf.sprintf "%s %s: dependency %s %s unsatisfied" n v d.dep_name d.req_str ]
      | (m, u) :: _ ->
          List.filter_map
            (fun f ->
              match entry cfg m u with
              | Some te when has_feature te f && f <> "default" ->
                  if List.mem f (feats_of (m, u)) then None
                  else Some (Printf.sprintf "%s %s: feature %s of %s not enabled" n v f d.dep_name)
              | _ -> None)
            d.dep_features
  in
  let feature_errors (n, v) e f =
    match List.assoc_opt f e.features with
    | None -> [ Printf.sprintf "%s %s: undeclared feature %s" n v f ]
    | Some specs ->
        List.concat_map
          (function
            | SFeature g ->
                if List.mem_assoc g e.features && not (List.mem g (feats_of (n, v))) then
                  [ Printf.sprintf "%s %s: feature %s missing %s" n v f g ]
                else []
            | SDep x | SDepFeature (x, _, false) -> (
                match find_dep e x with
                | Some d ->
                    if dep_applies d && selected d.dep_name d.req = [] then
                      [ Printf.sprintf "%s %s: feature %s needs %s" n v f x ]
                    else []
                | None -> [])
            | SDepFeature (_, _, true) -> [])
          specs
  in
  let pkg_errors (n, v) =
    match entry cfg n v with
    | None -> [ Printf.sprintf "%s %s not in index" n v ]
    | Some e ->
        let dev_applies d = d.kind = Dev && cfg_applies d in
        List.concat_map
          (fun d -> if d.optional then [] else dep_errors (n, v) d)
          e.deps
        @ (if List.mem n dev_roots then
             List.concat_map (dep_errors ~applies:dev_applies (n, v)) e.deps
           else [])
        @ List.concat_map (feature_errors (n, v) e) (feats_of (n, v))
  in
  uniqueness @ links_errors @ List.concat_map pkg_errors pkgs
