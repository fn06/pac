(* Alpine frontend: version constraints, !-conflicts, and namespaced virtual
   provides encoded to the core calculus as lazy lookups (the Debian pattern:
   shared selectors, per-declarer guards). *)

open Apk_index

module Name = struct
  type t =
    | N of string
    | Sel of string * string (* dependee name, constraint *)
    | Guard of string * string * string * string (* declarer name+ver, target, constraint *)
    | Trig of string * string (* install_if rule package *)
    | TGuard of string * string * int (* rule package, condition atom index *)

  let compare = Stdlib.compare

  let pp fmt = function
    | N n -> Format.pp_print_string fmt n
    | Sel (n, "") -> Format.fprintf fmt "<%s?>" n
    | Sel (n, c) -> Format.fprintf fmt "<%s%s>" n c
    | Guard (q, qv, t, c) -> Format.fprintf fmt "<%s.%s !! %s%s>" q qv t c
    | Trig (n, v) -> Format.fprintf fmt "<%s.%s if>" n v
    | TGuard (n, v, i) -> Format.fprintf fmt "<%s.%s if%d>" n v i
end

module Ver = struct
  type t =
    | B of int
    | P of int * string * Apk_version.t * string (* provider: priority, name, version *)
    | PR of Apk_version.t * string (* the real package *)
    | V of Apk_version.t * string

  let rank = function B _ -> 0 | P _ -> 1 | PR _ -> 2 | V _ -> 3

  let compare a b =
    match (a, b) with
    | V (x, _), V (y, _) | PR (x, _), PR (y, _) -> Apk_version.compare x y
    | P (i, n, x, _), P (j, m, y, _) ->
        let c = Stdlib.compare i j in
        if c <> 0 then c
        else
          let c = String.compare n m in
          if c <> 0 then c else Apk_version.compare x y
    | B i, B j -> Stdlib.compare j i
    | _ -> Stdlib.compare (rank a) (rank b)

  let pp fmt = function
    | V (_, s) | PR (_, s) -> Format.pp_print_string fmt s
    | P (_, n, _, s) -> Format.fprintf fmt "%s %s" n s
    | B i -> Format.pp_print_int fmt i
end

module Solver = Pubgrub.Make (Name) (Ver)

let of_list = Solver.Ranges.of_list

type instance = Apk_index.t

let real_versions (inst : instance) name rel =
  Option.value ~default:[] (Hashtbl.find_opt inst.by_name name)
  |> List.filter (fun p -> Apk_version.satisfies p.version rel)

(* an unversioned provides satisfies only unversioned constraints *)
let provider_matches rel pv =
  match (rel, pv) with
  | None, _ -> true
  | Some _, None -> false
  | Some (r, w), Some v -> Apk_version.satisfies v (Some (r, w))

let matching_providers (inst : instance) name rel =
  Hashtbl.find_all inst.providers name
  |> List.filter (fun (_, pv) -> provider_matches rel pv)

let sel_choices inst name rel =
  List.map (fun p -> Ver.PR (p.version, p.version_str)) (real_versions inst name rel)
  @ List.map
      (fun ((q : pkg), _) -> Ver.P (q.priority, q.name, q.version, q.version_str))
      (matching_providers inst name rel)

let atom_edges inst (a : atom) =
  match matching_providers inst a.a_name a.a_rel with
  | [] ->
      let vs =
        List.map (fun p -> Ver.V (p.version, p.version_str)) (real_versions inst a.a_name a.a_rel)
      in
      [ (Name.N a.a_name, of_list vs) ]
  | _ -> [ (Name.Sel (a.a_name, a.a_rel_str), of_list (sel_choices inst a.a_name a.a_rel)) ]

let guard_name (q : pkg) (a : atom) = Name.Guard (q.name, q.version_str, a.a_name, a.a_rel_str)
let same_id (q : pkg) (p : pkg) = q.name = p.name && q.version_str = p.version_str

(* does package p (as target [name] with its provides) satisfy atom a? *)
let target_matches (p : pkg) name (a : atom) =
  (a.a_name = name && Apk_version.satisfies p.version a.a_rel)
  || List.exists (fun (pn, pv) -> pn = a.a_name && provider_matches a.a_rel pv) p.provides

let dependencies (inst : instance) (n : Name.t) (v : Ver.t) : (Name.t * Solver.Ranges.t) list =
  let singleton x = of_list [ x ] in
  match (n, v) with
  | N name, V (_, vstr) -> (
      match Hashtbl.find_opt inst.by_id (name, vstr) with
      | None -> []
      | Some p ->
          let ifa_names = name :: List.map fst p.provides |> List.sort_uniq Stdlib.compare in
          List.concat_map (atom_edges inst) p.depends
          @ List.map (fun a -> (guard_name p a, singleton (Ver.B 1))) p.conflicts
          @ (Hashtbl.find_all inst.conflicts_against name
            |> List.filter (fun (q, a) ->
                   (not (same_id q p)) && Apk_version.satisfies p.version a.a_rel)
            |> List.map (fun (q, a) -> (guard_name q a, singleton (Ver.B 0))))
          @ (p.provides
            |> List.concat_map (fun (pn, pv) ->
                   Hashtbl.find_all inst.conflicts_against pn
                   |> List.filter (fun (q, a) ->
                          (not (same_id q p)) && provider_matches a.a_rel pv)
                   |> List.map (fun (q, a) -> (guard_name q a, singleton (Ver.B 0)))))
          (* satisfying an install_if condition atom kills that escape branch *)
          @ (ifa_names
            |> List.concat_map (Hashtbl.find_all inst.install_if_against)
            |> List.filter (fun ((r : pkg), _, a) ->
                   r.name <> name && target_matches p name a)
            |> List.sort_uniq Stdlib.compare
            |> List.map (fun ((r : pkg), i, _) ->
                   (Name.TGuard (r.name, r.version_str, i), singleton (Ver.B 0)))))
  | Sel (name, _), PR (dv, vstr) -> [ (Name.N name, singleton (Ver.V (dv, vstr))) ]
  | Sel _, P (_, pn, dv, vstr) -> [ (Name.N pn, singleton (Ver.V (dv, vstr))) ]
  | Trig (n, v), B i -> (
      match Hashtbl.find_opt inst.by_id (n, v) with
      | None -> []
      | Some r ->
          if i < List.length r.install_if then [ (Name.TGuard (n, v, i), singleton (Ver.B 1)) ]
          else [ (Name.N n, singleton (Ver.V (r.version, r.version_str))) ])
  | Guard _, B _ | TGuard _, B _ -> []
  | _ -> []

let versions (inst : instance) (n : Name.t) : Ver.t list =
  match n with
  | N name ->
      Option.value ~default:[] (Hashtbl.find_opt inst.by_name name)
      |> List.map (fun p -> Ver.V (p.version, p.version_str))
  | Sel (name, cstr) ->
      let rel = if cstr = "" then None else (Apk_index.parse_atom (name ^ cstr)).a_rel in
      sel_choices inst name rel
  | Guard _ | TGuard _ -> [ Ver.B 1; Ver.B 0 ]
  | Trig (n, v) -> (
      match Hashtbl.find_opt inst.by_id (n, v) with
      | None -> []
      | Some r ->
          (* escape branches first (preferred), the install branch last *)
          List.init (List.length r.install_if + 1) (fun j -> Ver.B j))

let solve inst (query : atom list) =
  let trigger_edges =
    List.map
      (fun (r : pkg) ->
        ( Name.Trig (r.name, r.version_str),
          of_list (List.init (List.length r.install_if + 1) (fun j -> Ver.B j)) ))
      inst.rules
  in
  let edges = List.concat_map (atom_edges inst) query @ trigger_edges in
  Solver.solve ~versions:(versions inst) ~dependencies:(dependencies inst) edges

let decode solution =
  List.filter_map (function Name.N n, Ver.V (_, s) -> Some (n, s) | _ -> None) solution
  |> List.sort Stdlib.compare

let check (inst : instance) resolution =
  let selected =
    List.filter_map (fun (n, v) -> Hashtbl.find_opt inst.by_id (n, v)) resolution
  in
  let names = List.map fst resolution in
  let uniqueness =
    if List.length (List.sort_uniq Stdlib.compare names) <> List.length names then
      [ "version uniqueness violated" ]
    else []
  in
  let satisfied_by (p : pkg) (a : atom) =
    (p.name = a.a_name && Apk_version.satisfies p.version a.a_rel)
    || List.exists (fun (pn, pv) -> pn = a.a_name && provider_matches a.a_rel pv) p.provides
  in
  let pkg_errors (p : pkg) =
    List.filter_map
      (fun a ->
        if List.exists (fun q -> satisfied_by q a) selected then None
        else
          Some
            (Printf.sprintf "%s-%s: unsatisfied dependency %s%s" p.name p.version_str a.a_name
               a.a_rel_str))
      p.depends
    @ List.concat_map
        (fun a ->
          List.filter_map
            (fun (t : pkg) ->
              if (not (same_id t p)) && satisfied_by t a then
                Some
                  (Printf.sprintf "%s-%s conflicts with %s-%s" p.name p.version_str t.name
                     t.version_str)
              else None)
            selected)
        p.conflicts
  in
  let rule_errors =
    List.filter_map
      (fun (r : pkg) ->
        if
          List.for_all
            (fun a -> List.exists (fun (q : pkg) -> target_matches q q.name a) selected)
            r.install_if
          && not (List.mem (r.name, r.version_str) resolution)
        then Some (Printf.sprintf "install_if violated: %s-%s should be installed" r.name r.version_str)
        else None)
      inst.rules
  in
  uniqueness @ List.concat_map pkg_errors selected @ rule_errors
