(* Debian frontend: the staged reduction to the core calculus, realised as
   lazy per-package lookups (paper §5.2) over an indexed Packages file, solved
   by unmodified pubgrub. *)

open Deb_packages

module Name = struct
  type t =
    | N of string (* real package name *)
    | Sel of string * string (* virtual selector: dependee name, constraint *)
    | Alt of string * string * int (* disjunct: owner name, owner version, group *)
    | Guard of string * string * string * string
      (* conflict guard: declarer name, declarer version, target, constraint *)

  let compare = Stdlib.compare

  let pp fmt = function
    | N n -> Format.pp_print_string fmt n
    | Sel (n, "") -> Format.fprintf fmt "<%s?>" n
    | Sel (n, c) -> Format.fprintf fmt "<%s %s>" n c
    | Alt (o, ov, i) -> Format.fprintf fmt "<%s %s |%d>" o ov i
    | Guard (q, qv, t, "") -> Format.fprintf fmt "<%s %s !! %s>" q qv t
    | Guard (q, qv, t, c) -> Format.fprintf fmt "<%s %s !! %s %s>" q qv t c
end

module Ver = struct
  type t =
    | B of int (* guard 0/1; alternative index, first preferred *)
    | P of string * Deb_version.t * string (* selector choice: provider *)
    | PR of Deb_version.t * string (* selector choice: the real package *)
    | V of Deb_version.t * string (* real version *)

  let rank = function B _ -> 0 | P _ -> 1 | PR _ -> 2 | V _ -> 3

  let compare a b =
    match (a, b) with
    | V (x, _), V (y, _) -> Deb_version.compare x y
    | PR (x, _), PR (y, _) -> Deb_version.compare x y
    | P (n, x, _), P (m, y, _) ->
        let c = String.compare n m in
        if c <> 0 then c else Deb_version.compare x y
    | B i, B j -> Stdlib.compare j i (* reversed: first alternative wins *)
    | _ -> Stdlib.compare (rank a) (rank b)

  let pp fmt = function
    | V (_, s) | PR (_, s) -> Format.pp_print_string fmt s
    | P (n, _, s) -> Format.fprintf fmt "%s %s" n s
    | B i -> Format.pp_print_int fmt i
end

module Solver = Pubgrub.Make (Name) (Ver)

type instance = {
  by_name : (string, pkg list) Hashtbl.t; (* newest first *)
  by_id : (string * string, pkg) Hashtbl.t;
  providers : (string, pkg * Deb_version.t option) Hashtbl.t;
  conflicts_against : (string, pkg * atom) Hashtbl.t;
}

let load pkgs =
  let by_name = Hashtbl.create 4096 in
  let by_id = Hashtbl.create 4096 in
  let providers = Hashtbl.create 1024 in
  let conflicts_against = Hashtbl.create 1024 in
  List.iter
    (fun p ->
      if not (Hashtbl.mem by_id (p.name, p.version_str)) then begin
        Hashtbl.replace by_id (p.name, p.version_str) p;
        Hashtbl.replace by_name p.name
          (p :: (Option.value ~default:[] (Hashtbl.find_opt by_name p.name)));
        List.iter (fun (n, v) -> Hashtbl.add providers n (p, v)) p.provides;
        List.iter (fun a -> Hashtbl.add conflicts_against a.dep_name (p, a)) p.conflicts
      end)
    pkgs;
  Hashtbl.filter_map_inplace
    (fun _ ps -> Some (List.sort (fun a b -> Deb_version.compare b.version a.version) ps))
    by_name;
  { by_name; by_id; providers; conflicts_against }

let real_versions inst name rel =
  Option.value ~default:[] (Hashtbl.find_opt inst.by_name name)
  |> List.filter (fun p -> satisfies p.version rel)

(* Policy 7.5: an unversioned Provides satisfies only unversioned dependencies. *)
let provider_matches rel pv =
  match (rel, pv) with
  | None, _ -> true
  | Some _, None -> false
  | Some r, Some v -> satisfies v (Some r)

let matching_providers inst name rel =
  Hashtbl.find_all inst.providers name
  |> List.filter (fun (_, pv) -> provider_matches rel pv)

let sel_choices inst name rel =
  List.map (fun p -> Ver.PR (p.version, p.version_str)) (real_versions inst name rel)
  @ List.map (fun (q, _) -> Ver.P (q.name, q.version, q.version_str))
      (matching_providers inst name rel)

let atom_edges inst a =
  match matching_providers inst a.dep_name a.dep_rel with
  | [] ->
      let vs =
        List.map (fun p -> Ver.V (p.version, p.version_str))
          (real_versions inst a.dep_name a.dep_rel)
      in
      [ (Name.N a.dep_name, Solver.Ranges.of_list vs) ]
  | _ ->
      [ ( Name.Sel (a.dep_name, a.dep_rel_str),
          Solver.Ranges.of_list (sel_choices inst a.dep_name a.dep_rel) ) ]

let parse_rel_str = function
  | "" -> None
  | s -> (
      match String.index_opt s ' ' with
      | Some i ->
          Some
            ( Deb_packages.parse_rel (String.sub s 0 i),
              Deb_version.parse (String.sub s (i + 1) (String.length s - i - 1)) )
      | None -> failwith ("bad constraint: " ^ s))

let guard_name (q : pkg) (a : atom) =
  Name.Guard (q.name, q.version_str, a.dep_name, a.dep_rel_str)

let same_id (q : pkg) (p : pkg) = q.name = p.name && q.version_str = p.version_str

let dependencies inst (n : Name.t) (v : Ver.t) : (Name.t * Solver.Ranges.t) list =
  let singleton x = Solver.Ranges.of_list [ x ] in
  match (n, v) with
  | N name, V (_, vstr) -> (
      match Hashtbl.find_opt inst.by_id (name, vstr) with
      | None -> []
      | Some p ->
          let dep_edges =
            List.concat
              (List.mapi
                 (fun i group ->
                   match group with
                   | [ a ] -> atom_edges inst a
                   | alts ->
                       [ ( Name.Alt (name, vstr, i),
                           Solver.Ranges.of_list (List.mapi (fun j _ -> Ver.B j) alts) )
                       ])
                 p.depends)
          in
          (* declared conflicts: require the guard high *)
          let guard_pos =
            List.map (fun a -> (guard_name p a, singleton (Ver.B 1))) p.conflicts
          in
          (* conflicts declared against this package, directly or via its
             Provides, excluding the declarer itself (Policy 7.5.2) *)
          let guard_neg =
            (Hashtbl.find_all inst.conflicts_against name
            |> List.filter (fun (q, a) -> (not (same_id q p)) && satisfies p.version a.dep_rel)
            |> List.map (fun (q, a) -> (guard_name q a, singleton (Ver.B 0))))
            @ (p.provides
              |> List.concat_map (fun (pn, pv) ->
                     Hashtbl.find_all inst.conflicts_against pn
                     |> List.filter (fun (q, a) ->
                            (not (same_id q p)) && provider_matches a.dep_rel pv)
                     |> List.map (fun (q, a) -> (guard_name q a, singleton (Ver.B 0)))))
          in
          dep_edges @ guard_pos @ guard_neg)
  | Sel (name, _), PR (dv, vstr) -> [ (Name.N name, singleton (Ver.V (dv, vstr))) ]
  | Sel _, P (pn, dv, vstr) -> [ (Name.N pn, singleton (Ver.V (dv, vstr))) ]
  | Alt (o, ov, i), B j -> (
      match Hashtbl.find_opt inst.by_id (o, ov) with
      | None -> []
      | Some p -> (
          match List.nth_opt p.depends i with
          | None -> []
          | Some group -> (
              match List.nth_opt group j with
              | None -> []
              | Some a -> atom_edges inst a)))
  | Guard _, B _ -> []
  | _ -> []

let versions inst (n : Name.t) : Ver.t list =
  match n with
  | N name ->
      Option.value ~default:[] (Hashtbl.find_opt inst.by_name name)
      |> List.map (fun p -> Ver.V (p.version, p.version_str))
  | Sel (name, cstr) -> sel_choices inst name (parse_rel_str cstr)
  | Alt (o, ov, i) -> (
      match Hashtbl.find_opt inst.by_id (o, ov) with
      | None -> []
      | Some p -> (
          match List.nth_opt p.depends i with
          | None -> []
          | Some group -> List.mapi (fun j _ -> Ver.B j) group))
  | Guard _ -> [ Ver.B 1; Ver.B 0 ]

let solve inst query_atoms =
  let query = List.concat_map (atom_edges inst) query_atoms in
  Solver.solve ~versions:(versions inst) ~dependencies:(dependencies inst) query

let decode solution =
  List.filter_map (function Name.N n, Ver.V (_, s) -> Some (n, s) | _ -> None) solution
  |> List.sort Stdlib.compare

(* Sanity check of a decoded resolution against the original Debian semantics:
   version uniqueness, every dependency group satisfied (directly or via a
   Provides), no conflict violated. An unverified precursor of the Lean checker. *)
let check inst resolution =
  let selected =
    List.filter_map (fun (n, vs) -> Hashtbl.find_opt inst.by_id (n, vs)) resolution
  in
  let names = List.map fst resolution in
  let uniqueness =
    if List.length (List.sort_uniq Stdlib.compare names) <> List.length names then
      [ "version uniqueness violated" ]
    else []
  in
  let atom_satisfied a =
    List.exists
      (fun p ->
        (p.name = a.dep_name && satisfies p.version a.dep_rel)
        || List.exists
             (fun (pn, pv) -> pn = a.dep_name && provider_matches a.dep_rel pv)
             p.provides)
      selected
  in
  let conflict_hits (t : pkg) a =
    (t.name = a.dep_name && satisfies t.version a.dep_rel)
    || List.exists (fun (pn, pv) -> pn = a.dep_name && provider_matches a.dep_rel pv) t.provides
  in
  let pkg_errors p =
    List.filter_map
      (fun group ->
        if List.exists atom_satisfied group then None
        else Some (Printf.sprintf "%s %s: unsatisfied dependency group" p.name p.version_str))
      p.depends
    @ List.concat_map
        (fun a ->
          List.filter_map
            (fun t ->
              if (not (same_id t p)) && conflict_hits t a then
                Some
                  (Printf.sprintf "%s %s conflicts with %s %s" p.name p.version_str t.name
                     t.version_str)
              else None)
            selected)
        p.conflicts
  in
  uniqueness @ List.concat_map pkg_errors selected
