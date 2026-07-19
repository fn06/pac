(* opam frontend: filtered package formulas encoded to the core calculus as
   lazy lookups. Variables go through the variable-formula reduction as
   synthetic packages -- pinned to an assignment, or solver-chosen when free. *)

open Opam_repo

let undefined = "%undefined%"

type config = {
  repo : Opam_repo.t;
  sigma : (string * string) list; (* variable assignment *)
  free : string list; (* solver-chosen variables *)
  root_overrides : (string * string) list;
      (* variables true only for the queried packages, e.g. with-test *)
  overrides : (string * string) list; (* the active substitution; [] outside shims *)
}

let default_sigma =
  [
    ("os", "linux");
    ("arch", "x86_64");
    ("opam-version", "2.4.0");
    ("with-test", "false");
    ("with-doc", "false");
    ("with-dev-setup", "false");
    ("dev", "false");
    ("build", "true");
    ("post", "true");
  ]

let sigma_get cfg x =
  match List.assoc_opt x cfg.sigma with Some v -> v | None -> undefined

let value_compare u y =
  if u = undefined then None
  else Some (Deb_version.compare (Deb_version.parse u) (Deb_version.parse y))

let holds rel u y =
  match value_compare u y with None -> false | Some c -> rel_holds rel c

(* ---------- elaborated formula ---------- *)

type guard_key = string * string * int (* declarer name, version, conflict atom index *)

type form =
  | True
  | False
  | Atom of string * (rel * string) list (* package, conjoined version constraints *)
  | Cmp of string * rel * string (* variable comparison *)
  | GAtom of guard_key * int (* conflict guard at value 0/1 *)
  | And of form list
  | Or of form list

(* Distinct variable literals of a filter conjunction. *)
let var_literals filters =
  let add acc l = if List.mem l acc then acc else l :: acc in
  let rec go acc = function
    | FBool _ | FVer _ -> acc
    | FVar x -> add acc (x, Eq, "true")
    | FCmp (x, r, y) -> add acc (x, r, y)
    | FNot f -> go acc f
    | FAnd (a, b) | FOr (a, b) -> go (go acc a) b
  in
  List.rev (List.fold_left go [] filters)

(* Reduce a filter under a truth assignment for its variable literals,
   leaving version constraints symbolic (as Atom with the package unfilled). *)
let rec reduce alpha = function
  | FBool b -> if b then True else False
  | FVer (r, v) -> Atom ("", [ (r, v) ])
  | FVar x -> if List.assoc (x, Eq, "true") alpha then True else False
  | FCmp (x, r, y) -> if List.assoc (x, r, y) alpha then True else False
  | FNot f -> neg alpha f
  | FAnd (a, b) -> (
      match (reduce alpha a, reduce alpha b) with
      | False, _ | _, False -> False
      | True, f | f, True -> f
      | f, g -> And [ f; g ])
  | FOr (a, b) -> (
      match (reduce alpha a, reduce alpha b) with
      | True, _ | _, True -> True
      | False, f | f, False -> f
      | f, g -> Or [ f; g ])

and neg alpha = function
  | FBool b -> if b then False else True
  | FVer (r, v) -> Atom ("", [ (complement r, v) ])
  | FVar x -> if List.assoc (x, Eq, "true") alpha then False else True
  | FCmp (x, r, y) -> if List.assoc (x, r, y) alpha then False else True
  | FNot f -> reduce alpha f
  | FAnd (a, b) -> (
      match (neg alpha a, neg alpha b) with
      | True, _ | _, True -> True
      | False, f | f, False -> f
      | f, g -> Or [ f; g ])
  | FOr (a, b) -> (
      match (neg alpha a, neg alpha b) with
      | False, _ | _, False -> False
      | True, f | f, True -> f
      | f, g -> And [ f; g ])

let rec fill_pkg n = function
  | Atom ("", cs) -> Atom (n, cs)
  | And fs -> And (List.map (fill_pkg n) fs)
  | Or fs -> Or (List.map (fill_pkg n) fs)
  | f -> f

let rec assignments = function
  | [] -> [ [] ]
  | l :: rest ->
      let tails = assignments rest in
      List.map (fun t -> ((l : string * rel * string), true) :: t) tails
      @ List.map (fun t -> (l, false) :: t) tails

let ground_literals cfg =
  let rec go = function
    | FVar x -> FBool (sigma_get cfg x = "true")
    | FCmp (x, r, y) -> FBool (holds r (sigma_get cfg x) y)
    | FNot f -> FNot (go f)
    | FAnd (a, b) -> FAnd (go a, go b)
    | FOr (a, b) -> FOr (go a, go b)
    | f -> f
  in
  go

(* Substitute root-scoped variables before branching, so their dependencies
   never touch the global ⟨x⟩ packages. *)
let presubst overrides f =
  let rec go = function
    | FVar x when List.mem_assoc x overrides ->
        FBool (List.assoc x overrides = "true")
    | FCmp (x, r, y) when List.mem_assoc x overrides ->
        FBool (holds r (List.assoc x overrides) y)
    | FNot f -> FNot (go f)
    | FAnd (a, b) -> FAnd (go a, go b)
    | FOr (a, b) -> FOr (go a, go b)
    | f -> f
  in
  go f

(* One dependency element "n" { filters }: branch over the truth of its
   variable literals; each branch conjoins the literals (as ⟨x⟩ constraints)
   with the reduced version formula. A branch whose filter reduces to false
   makes the dependency inapplicable, so the branch carries no atom. *)
let element cfg (n, filters) =
  let filters = List.map (presubst cfg.overrides) filters in
  let lits = var_literals filters in
  let conj = List.fold_left (fun acc f -> FAnd (acc, f)) (FBool true) filters in
  let branch alpha conj =
    let body = reduce alpha conj |> fill_pkg n in
    if body = False then True else if body = True then Atom (n, []) else body
  in
  match lits with
  | [] -> branch [] conj
  | _ when List.length lits > 10 ->
      (* safety valve: pre-evaluate the variables instead of branching *)
      branch [] (ground_literals cfg conj)
  | _ ->
      Or
        (List.map
           (fun alpha ->
             let cond =
               List.map
                 (fun ((x, r, y), b) ->
                   if b then Cmp (x, r, y) else Cmp (x, complement r, y))
                 alpha
             in
             match branch alpha conj with
             | True -> And cond
             | body -> And (cond @ [ body ]))
           (assignments lits))

(* Conflict atoms of a package, in a stable order. *)
let conflict_atoms (p : opkg) =
  let rec go acc = function
    | DAtom (n, fs) -> (n, fs) :: acc
    | DAnd l | DOr l -> List.fold_left go acc l
  in
  List.rev (go [] p.conflicts)

(* The variable conditions under which conflict atom filters [fs] match target
   version [v]: for each branch over the variable literals whose reduced
   version formula holds of v, the branch's literal conjunction. *)
let conflict_match_conds fs (v : Deb_version.t) =
  let lits = var_literals fs in
  let conj = List.fold_left (fun acc f -> FAnd (acc, f)) (FBool true) fs in
  let rec ver_holds = function
    | True -> true
    | False -> false
    | Atom (_, cs) ->
        List.for_all (fun (r, y) -> rel_holds r (Deb_version.compare v (Deb_version.parse y))) cs
    | And fs -> List.for_all ver_holds fs
    | Or fs -> List.exists ver_holds fs
    | Cmp _ | GAtom _ -> true
  in
  List.filter_map
    (fun alpha ->
      if ver_holds (reduce alpha conj) then
        Some
          (List.map
             (fun ((x, r, y), b) -> if b then Cmp (x, r, y) else Cmp (x, complement r, y))
             alpha)
      else None)
    (assignments lits)

let eval_ground_filter cfg f =
  let rec go = function
    | FBool b -> b
    | FVer _ -> true
    | FVar x -> sigma_get cfg x = "true"
    | FCmp (x, r, y) -> holds r (sigma_get cfg x) y
    | FNot f -> not (go f)
    | FAnd (a, b) -> go a && go b
    | FOr (a, b) -> go a || go b
  in
  go f

let available_versions cfg name =
  Opam_repo.versions cfg.repo name
  |> List.filter (fun p -> eval_ground_filter cfg p.available)

(* The full per-package form: depends elements plus conditional zero-edges for
   conflicts declared against it (same-name conflicts are left to version
   uniqueness). Deterministic, so Alt paths address it stably. *)
let deps_form cfg (p : opkg) =
  let rec elements = function
    | DAtom (n, fs) -> element cfg (n, fs)
    | DAnd l -> And (List.map elements l)
    | DOr l -> Or (List.map elements l)
  in
  elements p.depends

let full_form cfg (p : opkg) =
  let deps = deps_form cfg p in
  let pv = Deb_version.parse p.version in
  let incoming =
    Hashtbl.find_all cfg.repo.conflicts_against p.name
    |> List.filter (fun (q : opkg) -> q.name <> p.name)
    |> List.concat_map (fun (q : opkg) ->
           conflict_atoms q
           |> List.mapi (fun k (n, fs) -> (k, n, fs))
           |> List.filter (fun (_, n, _) -> n = p.name)
           |> List.concat_map (fun (k, _, fs) ->
                  conflict_match_conds fs pv
                  |> List.map (fun cond ->
                         (* under cond, the guard must be low *)
                         match cond with
                         | [] -> GAtom ((q.name, q.version, k), 0)
                         | _ ->
                             Or
                               (GAtom ((q.name, q.version, k), 0)
                               :: List.map
                                    (function
                                      | Cmp (x, r, y) -> Cmp (x, complement r, y)
                                      | _ -> assert false)
                                    cond))))
  in
  And (deps :: incoming)

let rec sub_form f path =
  match (path, f) with
  | [], f -> f
  | i :: rest, (And fs | Or fs) -> sub_form (List.nth fs i) rest
  | _ -> invalid_arg "sub_form"

(* ---------- solver instantiation ---------- *)

module Name = struct
  type t =
    | N of string
    | Var of string
    | Alt of string * string * int list
    | R of string (* root shim: a queried package's deps under root_overrides *)
    | RAlt of string * string * int list (* Alt within a root shim's form *)
    | Guard of guard_key
    | ClassGuard of string * string (* class, member name *)

  let compare = Stdlib.compare

  let pp fmt = function
    | N n -> Format.pp_print_string fmt n
    | Var x -> Format.fprintf fmt "<%s>" x
    | Alt (n, v, path) ->
        Format.fprintf fmt "<%s.%s |%s>" n v
          (String.concat "." (List.map string_of_int path))
    | R n -> Format.fprintf fmt "<root:%s>" n
    | RAlt (n, v, path) ->
        Format.fprintf fmt "<root:%s.%s |%s>" n v
          (String.concat "." (List.map string_of_int path))
    | Guard (q, qv, k) -> Format.fprintf fmt "<%s.%s !!%d>" q qv k
    | ClassGuard (c, m) -> Format.fprintf fmt "<class %s %s>" c m
end

module Ver = struct
  type t = B of int | Val of string | V of Deb_version.t * string

  let rank = function B _ -> 0 | Val _ -> 1 | V _ -> 2

  let compare a b =
    match (a, b) with
    | V (x, _), V (y, _) -> Deb_version.compare x y
    | Val x, Val y -> Deb_version.compare (Deb_version.parse x) (Deb_version.parse y)
    | B i, B j -> Stdlib.compare j i (* reversed: leftmost branch preferred *)
    | _ -> Stdlib.compare (rank a) (rank b)

  let pp fmt = function
    | V (_, s) | Val s -> Format.pp_print_string fmt s
    | B i -> Format.pp_print_int fmt i
end

module Solver = Pubgrub.Make (Name) (Ver)

let domain cfg x =
  if List.mem x cfg.free then
    match Opam_repo.domain cfg.repo x with [] -> [ "true"; "false" ] | vs -> vs
  else [ sigma_get cfg x ]

let rec encode cfg owner path (f : form) : (Name.t * Solver.Ranges.t) list =
  let of_list = Solver.Ranges.of_list in
  match f with
  | True -> []
  | False -> [ (Name.N "%unsatisfiable%", of_list []) ]
  | Atom (n, cs) ->
      let vs =
        available_versions cfg n
        |> List.filter (fun p ->
               let pv = Deb_version.parse p.version in
               List.for_all
                 (fun (r, y) -> rel_holds r (Deb_version.compare pv (Deb_version.parse y)))
                 cs)
        |> List.map (fun p -> Ver.V (Deb_version.parse p.version, p.version))
      in
      [ (Name.N n, of_list vs) ]
  | Cmp (x, r, y) ->
      let dom = domain cfg x in
      [ ( Name.Var x,
          of_list (List.filter_map (fun u -> if holds r u y then Some (Ver.Val u) else None) dom)
        ) ]
  | GAtom (g, i) -> [ (Name.Guard g, of_list [ Ver.B i ]) ]
  | And fs -> List.concat (List.mapi (fun i f -> encode cfg owner (path @ [ i ]) f) fs)
  | Or fs ->
      let n, v = owner in
      let alt = if cfg.overrides = [] then Name.Alt (n, v, path) else Name.RAlt (n, v, path) in
      [ (alt, of_list (List.mapi (fun j _ -> Ver.B j) fs)) ]

let dependencies cfg (n : Name.t) (v : Ver.t) : (Name.t * Solver.Ranges.t) list =
  match (n, v) with
  | N name, V (_, vstr) -> (
      match Hashtbl.find_opt cfg.repo.by_id (name, vstr) with
      | None -> []
      | Some p ->
          let form_edges = encode cfg (name, vstr) [] (full_form cfg p) in
          let guard_edges =
            conflict_atoms p
            |> List.mapi (fun k (tn, _) -> (k, tn))
            |> List.filter (fun (_, tn) -> tn <> p.name)
            |> List.map (fun (k, _) ->
                   (Name.Guard (p.name, p.version, k), Solver.Ranges.of_list [ Ver.B 1 ]))
          in
          let class_edges =
            p.conflict_classes
            |> List.concat_map (fun c ->
                   (Name.ClassGuard (c, p.name), Solver.Ranges.of_list [ Ver.B 1 ])
                   :: (Hashtbl.find_all cfg.repo.class_members c
                      |> List.map (fun (m : opkg) -> m.name)
                      |> List.filter (( <> ) p.name)
                      |> List.sort_uniq Stdlib.compare
                      |> List.map (fun m ->
                             (Name.ClassGuard (c, m), Solver.Ranges.of_list [ Ver.B 0 ]))))
          in
          form_edges @ guard_edges @ class_edges)
  | Alt (o, ov, path), B j -> (
      match Hashtbl.find_opt cfg.repo.by_id (o, ov) with
      | None -> []
      | Some p -> (
          match sub_form (full_form cfg p) path with
          | Or fs -> encode cfg (o, ov) (path @ [ j ]) (List.nth fs j)
          | _ -> []))
  | R name, V (_, vstr) -> (
      (* the queried package's depends under root_overrides, agreeing with
         its version choice; conflicts stay with the plain package *)
      match Hashtbl.find_opt cfg.repo.by_id (name, vstr) with
      | None -> []
      | Some p ->
          let cfg = { cfg with overrides = cfg.root_overrides } in
          (Name.N name, Solver.Ranges.of_list [ v ])
          :: encode cfg (name, vstr) [] (deps_form cfg p))
  | RAlt (o, ov, path), B j -> (
      match Hashtbl.find_opt cfg.repo.by_id (o, ov) with
      | None -> []
      | Some p -> (
          let cfg = { cfg with overrides = cfg.root_overrides } in
          match sub_form (deps_form cfg p) path with
          | Or fs -> encode cfg (o, ov) (path @ [ j ]) (List.nth fs j)
          | _ -> []))
  | _ -> []

let versions cfg (n : Name.t) : Ver.t list =
  match n with
  | N name ->
      available_versions cfg name
      |> List.map (fun p -> Ver.V (Deb_version.parse p.version, p.version))
  | Var x -> List.map (fun u -> Ver.Val u) (domain cfg x)
  | Alt (o, ov, path) -> (
      match Hashtbl.find_opt cfg.repo.by_id (o, ov) with
      | None -> []
      | Some p -> (
          match sub_form (full_form cfg p) path with
          | Or fs -> List.mapi (fun j _ -> Ver.B j) fs
          | _ -> []))
  | R name ->
      available_versions cfg name
      |> List.map (fun p -> Ver.V (Deb_version.parse p.version, p.version))
  | RAlt (o, ov, path) -> (
      match Hashtbl.find_opt cfg.repo.by_id (o, ov) with
      | None -> []
      | Some p -> (
          let cfg = { cfg with overrides = cfg.root_overrides } in
          match sub_form (deps_form cfg p) path with
          | Or fs -> List.mapi (fun j _ -> Ver.B j) fs
          | _ -> []))
  | Guard _ | ClassGuard _ -> [ Ver.B 1; Ver.B 0 ]

let solve cfg (query : (string * (rel * string) option) list) =
  let query_edges =
    List.concat_map
      (fun (n, c) ->
        let base = encode cfg ("%root%", "") [] (Atom (n, Option.to_list c)) in
        if cfg.root_overrides = [] then base
        else base @ List.map (function Name.N m, vs -> (Name.R m, vs) | e -> e) base)
      query
  in
  Solver.solve ~versions:(versions cfg) ~dependencies:(dependencies cfg) query_edges

let decode cfg solution =
  let pkgs =
    List.filter_map (function Name.N n, Ver.V (_, s) -> Some (n, s) | _ -> None) solution
    |> List.sort Stdlib.compare
  in
  let free_choices =
    List.filter_map
      (function
        | Name.Var x, Ver.Val v when List.mem x cfg.free -> Some (x, v) | _ -> None)
      solution
    |> List.sort Stdlib.compare
  in
  (pkgs, free_choices)

(* ---------- checker: decoded resolution vs elaborated opam semantics ---------- *)

let check ?(scoped_roots = []) cfg (pkgs : (string * string) list)
    (free_choices : (string * string) list) =
  let cfg = { cfg with sigma = free_choices @ cfg.sigma; free = [] } in
  let names = List.map fst pkgs in
  let uniqueness =
    if List.length (List.sort_uniq Stdlib.compare names) <> List.length names then
      [ "version uniqueness violated" ]
    else []
  in
  let selected = List.filter_map (fun (n, v) -> Hashtbl.find_opt cfg.repo.by_id (n, v)) pkgs in
  let sel_version n =
    List.find_map (fun (m, v) -> if m = n then Some (Deb_version.parse v) else None) pkgs
  in
  let rec eval_form = function
    | True -> true
    | False -> false
    | Atom (n, cs) -> (
        match sel_version n with
        | None -> false
        | Some v ->
            List.for_all
              (fun (r, y) -> rel_holds r (Deb_version.compare v (Deb_version.parse y)))
              cs)
    | Cmp (x, r, y) -> holds r (sigma_get cfg x) y
    | GAtom _ -> true
    | And fs -> List.for_all eval_form fs
    | Or fs -> List.exists eval_form fs
  in
  let pkg_errors (p : opkg) =
    let dcfg =
      if List.mem p.name scoped_roots then { cfg with overrides = cfg.root_overrides }
      else cfg
    in
    let rec eval_deps = function
      | DAtom (n, fs) -> eval_form (element dcfg (n, fs))
      | DAnd l -> List.for_all eval_deps l
      | DOr l -> List.exists eval_deps l
    in
    (if eval_deps p.depends then []
     else [ Printf.sprintf "%s.%s: unsatisfied dependency" p.name p.version ])
    @ List.concat_map
        (fun (t : opkg) ->
          if t.name = p.name then []
          else
            (conflict_atoms p
            |> List.filter_map (fun (n, fs) ->
                   if
                     n = t.name
                     && conflict_match_conds fs (Deb_version.parse t.version)
                        |> List.exists (List.for_all eval_form)
                   then
                     Some
                       (Printf.sprintf "%s.%s conflicts with %s.%s" p.name p.version t.name
                          t.version)
                   else None))
            @
            if List.exists (fun c -> List.mem c t.conflict_classes) p.conflict_classes then
              [ Printf.sprintf "%s.%s and %s.%s share a conflict-class" p.name p.version
                  t.name t.version ]
            else [])
        selected
  in
  uniqueness @ List.concat_map pkg_errors selected
