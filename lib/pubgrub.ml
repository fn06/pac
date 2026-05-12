open Import
open Option.Syntax

module type NAME = Ranges.ORDERED
module type VERSION = Ranges.ORDERED

let debug_enabled = ref false

let debug_printf fmt =
  if !debug_enabled then
    Format.kfprintf
      (fun _ -> Format.pp_print_flush Format.std_formatter ())
      Format.std_formatter fmt
  else Format.ifprintf Format.std_formatter fmt

let set_debug enabled = debug_enabled := enabled

module Ranges_mod = Ranges

module Make (N : NAME) (V : VERSION) = struct
  module Ranges = Ranges_mod.Make (V)

  type name = RootName | Name of N.t
  type version = RootVersion | Version of V.t
  type package = name * version

  let compare_name a b =
    match (a, b) with
    | RootName, RootName -> 0
    | RootName, _ -> -1
    | _, RootName -> 1
    | Name a, Name b -> N.compare a b

  let compare_version a b =
    match (a, b) with
    | RootVersion, RootVersion -> 0
    | RootVersion, _ -> -1
    | _, RootVersion -> 1
    | Version a, Version b -> V.compare a b

  let pp_name fmt = function
    | RootName -> Format.pp_print_string fmt "Root"
    | Name n -> N.pp fmt n

  let pp_version fmt = function
    | RootVersion -> Format.pp_print_string fmt "Root"
    | Version v -> V.pp fmt v

  let pp_package fmt = function
    | RootName, RootVersion -> Format.fprintf fmt "root"
    | n, v -> Format.fprintf fmt "%a %a" pp_name n pp_version v

  (* Internal ranges over the version type (includes RootVersion) *)
  module IV = struct
    type t = version

    let compare = compare_version
    let pp = pp_version
  end

  module R = Ranges_mod.Make (IV)

  (* Convert external Ranges.t (over V.t) to internal R.t (over version) *)
  let internalize_bound = function
    | Ranges.Unbounded -> R.Unbounded
    | Ranges.Included v -> R.Included (Version v)
    | Ranges.Excluded v -> R.Excluded (Version v)

  let internalize_range (r : Ranges.t) : R.t =
    List.map
      (fun ((lo, hi) : Ranges.segment) ->
        ((internalize_bound lo, internalize_bound hi) : R.segment))
      r

  let root_singleton = R.singleton RootVersion

  type polarity = Pos | Neg
  type term = polarity * name * R.t
  type dependency = package * (name * R.t)

  type cause =
    | RootCause
    | NoVersions
    | Dependency of dependency
    | Derived of incompatibility * incompatibility

  and incompatibility = { terms : term list; cause : cause }

  type decision_level = int
  type assignment = Decision of package | Derivation of term * incompatibility
  type solution = (assignment * decision_level) list

  type state = {
    incomps : incompatibility list;
    solution : solution;
    decision_level : decision_level;
  }

  let pp_polarity fmt = function
    | Pos -> Format.fprintf fmt "%s" ""
    | Neg -> Format.fprintf fmt "%s" "not "

  let pp_term fmt (p, n, vs) =
    Format.fprintf fmt "%a%a %a" pp_polarity p pp_name n R.pp vs

  let pp_terms fmt terms =
    Format.fprintf fmt "{%a}"
      Format.(
        pp_print_list
          ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
          (fun fmt t -> fprintf fmt "%a" pp_term t))
      terms

  let rec pp_cause fmt = function
    | RootCause -> Format.pp_print_string fmt "root"
    | NoVersions -> Format.pp_print_string fmt "no versions"
    | Dependency (pkg, (n, r)) ->
        Format.fprintf fmt "dependency %a -> %a %a" pp_package pkg pp_name n R.pp r
    | Derived (i1, i2) ->
        Format.fprintf fmt "(%a and %a)" pp_incompatibility i1 pp_incompatibility i2

  and pp_incompatibility fmt { terms; cause } =
    Format.fprintf fmt "(terms: %a, cause: %a)" pp_terms terms pp_cause cause

  let pp_incompatibilities fmt incomps =
    Format.fprintf fmt "%a"
      Format.(
        pp_print_list
          ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "\n\t")
          (fun fmt i -> fprintf fmt "%a" pp_incompatibility i))
      incomps

  let pp_assignment fmt = function
    | Decision package -> Format.fprintf fmt "Decision %a" pp_package package
    | Derivation (term, cause) ->
        Format.fprintf fmt "Derivation %a due to incompatibility %a" pp_term term
          pp_incompatibility cause

  let _pp_solution fmt =
    Format.(
      pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        (fun fmt (a, d) -> fprintf fmt "(%d: %a)" d pp_assignment a))
      fmt

  let term_name = function _, name, _ -> name

  let negate_term = function
    | Pos, name, r -> (Neg, name, r)
    | Neg, name, r -> (Pos, name, r)

  let term_satisfies (sp, _, sr) (tp, _, tr) =
    match (sp, tp) with
    | Pos, Pos -> R.subset_of sr tr
    | Neg, Neg -> R.subset_of tr sr
    | Pos, Neg -> R.is_disjoint sr tr
    | Neg, Pos -> false

  (* not (a \ b) viewed as version sets *)
  let term_not_difference (sp, sn, sr) (tp, _, tr) =
    match (sp, tp) with
    | Pos, Pos -> (Neg, sn, R.difference sr tr)
    | Neg, Pos -> (Pos, sn, R.union sr tr)
    | Pos, Neg -> (Neg, sn, R.intersection sr tr)
    | Neg, Neg -> (Neg, sn, R.difference tr sr)

  (* Compute the effective range for a name from the solution, and whether
     there's any positive derivation for it. *)
  let solution_range name solution =
    let rec aux has_pos = function
      | [] -> (has_pos, R.full)
      | (Decision (n, v), _) :: _ when compare_name n name = 0 -> (true, R.singleton v)
      | (Derivation ((Pos, n, r), _), _) :: rest when compare_name n name = 0 ->
          let _, sr = aux true rest in
          (true, R.intersection r sr)
      | (Derivation ((Neg, n, r), _), _) :: rest when compare_name n name = 0 ->
          let has_pos, sr = aux has_pos rest in
          (has_pos, R.intersection (R.complement r) sr)
      | _ :: rest -> aux has_pos rest
    in
    aux false solution

  let term_status solution (pol, name, vs) =
    let has_positive, sr = solution_range name solution in
    match (has_positive, pol) with
    | false, Pos -> `Contradicted
    | false, Neg -> if R.is_disjoint sr vs then `Satisfied else `Undetermined
    | true, _ ->
        if R.subset_of sr vs then
          match pol with Pos -> `Satisfied | Neg -> `Contradicted
        else if R.is_disjoint sr vs then
          match pol with Pos -> `Contradicted | Neg -> `Satisfied
        else `Undetermined

  let incompatibility_status solution incomp =
    let rec aux s = function
      | [] -> s
      | t :: ts -> (
          match (s, term_status solution t) with
          | `Satisfied, `Satisfied -> aux `Satisfied ts
          | `Satisfied, `Undetermined -> aux (`Almost_satisfied t) ts
          | `Almost_satisfied t, `Satisfied -> aux (`Almost_satisfied t) ts
          | `Almost_satisfied _, `Undetermined -> aux `Undetermined ts
          | `Contradicted, _ -> `Contradicted
          | _, `Contradicted -> `Contradicted
          | `Undetermined, _ -> aux `Undetermined ts)
    in
    aux `Satisfied incomp.terms

  let normalise_terms terms =
    let tbl = Hashtbl.create (List.length terms) in
    List.iter
      (function
        | Neg, RootName, _ -> ()
        | pol, name, r -> (
            let key =
              List.find_opt
                (fun k -> compare_name k name = 0)
                (List.of_seq (Hashtbl.to_seq_keys tbl))
            in
            let key = match key with Some k -> k | None -> name in
            let replace = Hashtbl.replace tbl key in
            match Hashtbl.find_opt tbl key with
            | None -> replace (pol, r)
            | Some (pol', r') -> (
                match (pol, pol') with
                | Pos, Pos | Neg, Neg -> replace (pol, R.intersection r r')
                | Pos, Neg | Neg, Pos -> replace (Pos, if pol = Pos then r else r'))))
      terms;
    Hashtbl.fold (fun name (pol, r) acc -> (pol, name, r) :: acc) tbl []

  let rec conflict_resolution state original_incomp incomp :
      (state * incompatibility * term, incompatibility) Result.t =
    debug_printf "conflict resolution on: %a\n" pp_incompatibility incomp;
    let rec find_earliest_satisfier incomp = function
      | [] -> []
      | assignment :: assignments -> (
          match find_earliest_satisfier incomp assignments with
          | [] -> (
              match incompatibility_status (assignment :: assignments) incomp with
              | `Satisfied -> assignment :: assignments
              | _ -> [])
          | solution -> solution)
    in
    let rec find_previous_satisfier satisfier incomp = function
      | [] -> []
      | assignment :: assignments -> (
          match find_previous_satisfier satisfier incomp assignments with
          | [] -> (
              match
                incompatibility_status (satisfier :: assignment :: assignments) incomp
              with
              | `Satisfied -> assignment :: assignments
              | _ -> [])
          | solution -> solution)
    in
    match incomp.terms with
    | [] -> Error incomp
    | [ (Pos, RootName, r) ] when R.subset_of r root_singleton -> Error incomp
    | _ -> (
        let (satisfier, satisfier_decision_level), assignments =
          match find_earliest_satisfier incomp state.solution with
          | assignment :: assignments -> (assignment, assignments)
          | _ -> failwith "Incompatibility not satisfied"
        in
        debug_printf "satisfiying assignment on level %d: %a\n" satisfier_decision_level
          pp_assignment satisfier;
        let term =
          let name =
            match satisfier with
            | Decision (name, _) -> name
            | Derivation ((_, name, _), _) -> name
          in
          List.find (fun t -> compare_name (term_name t) name = 0) incomp.terms
        in
        let previous_satisfier_level =
          match
            find_previous_satisfier
              (satisfier, satisfier_decision_level)
              incomp assignments
          with
          | (_, decision_level) :: _ -> decision_level
          | _ -> 1
        in
        match (satisfier, satisfier_decision_level != previous_satisfier_level) with
        | Decision _, _ | _, true ->
            debug_printf "backtracking to level %d\n" previous_satisfier_level;
            let solution =
              List.filter
                (fun (_assignment, decision_level) ->
                  decision_level <= previous_satisfier_level)
                state.solution
            in
            let incomps =
              if incomp != original_incomp then (
                debug_printf "new incompatibility %a\n" pp_incompatibility incomp;
                incomp :: state.incomps)
              else state.incomps
            in
            let state =
              { incomps; solution; decision_level = previous_satisfier_level }
            in
            Ok (state, incomp, term)
        | Derivation (satisfier_term, cause), _ ->
            let base_terms =
              incomp.terms @ cause.terms
              |> List.filter (fun t -> compare_name (term_name t) (term_name term) <> 0)
            in
            let partial_satisfier_term =
              if term_satisfies satisfier_term term then []
              else [ term_not_difference satisfier_term term ]
            in
            let prior_cause =
              {
                terms = normalise_terms (base_terms @ partial_satisfier_term);
                cause = Derived (incomp, cause);
              }
            in
            debug_printf "prior cause %a\n" pp_incompatibility prior_cause;
            conflict_resolution state original_incomp prior_cause)

  let rec unit_propagation state changed : (state, incompatibility) Result.t =
    match changed with
    | [] -> Ok state
    | name :: changed ->
        debug_printf "unit propagation on: %a\n" pp_name name;
        let incomps =
          List.filter
            (fun incomp ->
              List.exists (fun t -> compare_name (term_name t) name = 0) incomp.terms)
            state.incomps
        in
        incompat_propagation state changed incomps

  and incompat_propagation state changed = function
    | [] -> unit_propagation state changed
    | incomp :: incomps -> (
        match incompatibility_status state.solution incomp with
        | `Satisfied -> (
            match conflict_resolution state incomp incomp with
            | Ok (state, incomp, term) ->
                let assignment = Derivation (negate_term term, incomp) in
                let _, name, _ = term in
                debug_printf "new assignment on level %d: %a\n" state.decision_level
                  pp_assignment assignment;
                let state =
                  {
                    state with
                    solution = (assignment, state.decision_level) :: state.solution;
                  }
                in
                unit_propagation state [ name ]
            | Error incomp -> Error incomp)
        | `Almost_satisfied term ->
            let assignment = Derivation (negate_term term, incomp) in
            debug_printf "new assignment on level %d: %a\n" state.decision_level
              pp_assignment assignment;
            let solution = (assignment, state.decision_level) :: state.solution in
            let state = { state with solution } in
            let _, name, _ = term in
            incompat_propagation state (name :: changed) incomps
        | _ -> incompat_propagation state changed incomps)

  let dependency_incomps dependency_map version_map (name, version) =
    List.map
      (fun (dep_name, dep_range) ->
        let depender_versions =
          Hashtbl.find_all version_map name
          |> List.filter (fun v ->
                 List.exists
                   (fun (dn, dr) -> compare_name dn dep_name = 0 && dr = dep_range)
                   (Hashtbl.find_all dependency_map (name, v)))
        in
        let depender_range = R.of_list depender_versions in
        {
          terms = [ (Pos, name, depender_range); (Neg, dep_name, dep_range) ];
          cause = Dependency ((name, version), (dep_name, dep_range));
        })
      (Hashtbl.find_all dependency_map (name, version))

  let make_decision version_map dependency_map state =
    let find_undecided_term () =
      let rec aux best = function
        | [] -> best
        | (Derivation ((Pos, name, _), _), _) :: solution
          when compare_name name RootName <> 0 ->
            let _, sr = solution_range name state.solution in
            let real_vs =
              List.filter (fun v -> R.contains v sr) (Hashtbl.find_all version_map name)
            in
            let decided =
              List.exists
                (fun (a, _) ->
                  match a with Decision (n, _) -> compare_name n name = 0 | _ -> false)
                state.solution
            in
            if decided then aux best solution
            else
              let n = List.length real_vs in
              let best =
                match best with
                | Some (_, _, c) when c <= n -> best
                | _ -> Some (name, real_vs, n)
              in
              aux best solution
        | _ :: solution -> aux best solution
      in
      aux None state.solution |> Option.map (fun (name, vs, _) -> (name, vs))
    in
    let* name, real_vs = find_undecided_term () in
    let _, sr = solution_range name state.solution in
    debug_printf "deciding on %a: %a\n" pp_name name R.pp sr;
    let decision_level = state.decision_level + 1 in
    match real_vs with
    | [] ->
        let incomp = { terms = [ (Pos, name, sr) ]; cause = NoVersions } in
        debug_printf "no versions found, adding incompatiblity %a\n" pp_incompatibility
          incomp;
        let state = { state with incomps = incomp :: state.incomps } in
        Some (name, state)
    | _ ->
        let rec try_versions state = function
          | [] -> Some (name, state)
          | version :: versions -> (
              debug_printf "trying version %a\n" pp_version version;
              let dep_incomps =
                dependency_incomps dependency_map version_map (name, version)
                |> List.filter (fun i ->
                       not (List.exists (fun i' -> i'.terms = i.terms) state.incomps))
              in
              if List.length dep_incomps > 0 then
                debug_printf "dependency incompatibilities\n\t%a\n" pp_incompatibilities
                  dep_incomps;
              let incomps = dep_incomps @ state.incomps in
              let state = { state with incomps } in
              let assignment = Decision (name, version) in
              let solution = (assignment, decision_level) :: state.solution in
              match
                List.find_opt
                  (fun i ->
                    match incompatibility_status solution i with
                    | `Satisfied -> true
                    | _ -> false)
                  incomps
              with
              | Some incomp ->
                  debug_printf "not adding due to incompatibility %a\n" pp_incompatibility
                    incomp;
                  try_versions state versions
              | None ->
                  debug_printf "assignment on level %d: %a\n" decision_level pp_assignment
                    assignment;
                  let state = { incomps; solution; decision_level } in
                  Some (name, state))
        in
        try_versions state (List.sort (fun a b -> compare_version b a) real_vs)

  let extract_resolution state =
    List.filter_map (function Decision pkg, _ -> Some pkg | _ -> None) state.solution

  let init_incomps dependency_map =
    { terms = [ (Neg, RootName, root_singleton) ]; cause = RootCause }
    :: List.map
         (fun ((dep_name, dep_range) as dep) ->
           {
             terms = [ (Pos, RootName, root_singleton); (Neg, dep_name, dep_range) ];
             cause = Dependency ((RootName, RootVersion), dep);
           })
         (Hashtbl.find_all dependency_map (RootName, RootVersion))

  type repository = (N.t * V.t) list
  type dependencies = ((N.t * V.t) * (N.t * Ranges.t)) list
  type query = (N.t * Ranges.t) list

  let resolve (repository : repository) (dependencies : dependencies) (query : query) :
      ((N.t * V.t) list, incompatibility) Result.t =
    let version_map = Hashtbl.create 0 in
    List.iter
      (fun (name, version) -> Hashtbl.add version_map (Name name) (Version version))
      repository;
    let dependency_map = Hashtbl.create 0 in
    List.iter
      (fun ((n, v), (dep_name, dep_range)) ->
        Hashtbl.add dependency_map (Name n, Version v)
          (Name dep_name, internalize_range dep_range))
      dependencies;
    List.iter
      (fun (name, range) ->
        Hashtbl.add dependency_map (RootName, RootVersion)
          (Name name, internalize_range range))
      query;
    let rec solve_loop state next =
      match unit_propagation state [ next ] with
      | Error incomp -> Error incomp
      | Ok state -> (
          match make_decision version_map dependency_map state with
          | None -> Ok (extract_resolution state)
          | Some (next, state) -> solve_loop state next)
    in
    let incomps = init_incomps dependency_map in
    debug_printf "initial incompatibilities\n\t%a\n" pp_incompatibilities incomps;
    solve_loop { incomps; solution = []; decision_level = 0 } RootName
    |> Result.map
         (List.filter_map (function Name n, Version v -> Some (n, v) | _ -> None))

  let explain_terms fmt = function
    | [ (Pos, n, vs); (Neg, m, us) ] | [ (Neg, m, us); (Pos, n, vs) ] ->
        Format.fprintf fmt "%a %a requires %a %a" pp_name n R.pp vs pp_name m R.pp us
    | [] | [ (Pos, RootName, _) ] -> Format.fprintf fmt "version solving failed."
    | terms ->
        Format.fprintf fmt "%a is forbidden."
          Format.(
            pp_print_list
              ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " or ")
              (fun fmt t -> fprintf fmt "%a" pp_term t))
          terms

  let explain_incompatibility fmt root =
    let line_numbers = Hashtbl.create 16 in
    let line_number = ref 0 in
    let set_line_number cause =
      incr line_number;
      Hashtbl.add line_numbers cause !line_number;
      !line_number
    in
    let is_external incomp = match incomp.cause with Derived _ -> false | _ -> true in
    let rec count_caused incomp = function
      | Derived (c1, c2) ->
          (if c1 == incomp then 1 else 0)
          + (if c2 == incomp then 1 else 0)
          + count_caused incomp c1.cause + count_caused incomp c2.cause
      | _ -> 0
    in
    let rec explain_incomp fmt incomp =
      match incomp.cause with
      | RootCause -> Format.fprintf fmt "root"
      | NoVersions -> Format.fprintf fmt "%a not available" explain_terms incomp.terms
      | Dependency (pkg, (n, r)) ->
          Format.fprintf fmt "%a -> %a %a" pp_package pkg pp_name n R.pp r
      | Derived (cause1, cause2) ->
          (match (is_external cause1, is_external cause2) with
          | false, false -> (
              match
                ( Hashtbl.find_opt line_numbers cause1,
                  Hashtbl.find_opt line_numbers cause2 )
              with
              | Some line1, Some line2 ->
                  Format.fprintf fmt "Because %a (%d) and %a (%d), %a." explain_terms
                    cause1.terms line1 explain_terms cause2.terms line2 explain_terms
                    incomp.terms
              | Some line1, None ->
                  Format.fprintf fmt "%a\nAnd because %a (%d), %a." explain_incomp cause2
                    explain_terms cause1.terms line1 explain_terms incomp.terms
              | None, Some line2 ->
                  Format.fprintf fmt "%a\nAnd because %a (%d), %a." explain_incomp cause1
                    explain_terms cause2.terms line2 explain_terms incomp.terms
              | None, None -> (
                  let is_simple incomp =
                    match incomp.cause with
                    | Derived (c1, c2) -> is_external c1 && is_external c2
                    | _ -> true
                  in
                  match
                    match (is_simple cause1, is_simple cause2) with
                    | true, _ -> Some (cause1, cause2)
                    | false, true -> Some (cause2, cause1)
                    | false, false -> None
                  with
                  | Some (simple, complex) ->
                      Format.fprintf fmt "%a\n%a\nThus, %a" explain_incomp complex
                        explain_incomp simple explain_terms incomp.terms
                  | None ->
                      let line1 = set_line_number cause1 in
                      let line2 = set_line_number cause2 in
                      Format.fprintf fmt "%a (%d)\n\n%a (%d)\nThus, %a" explain_incomp
                        cause1 line1 explain_incomp cause2 line2 explain_terms
                        incomp.terms))
          | false, _ | _, false -> (
              let derived, ext =
                if is_external cause1 then (cause2, cause1) else (cause1, cause2)
              in
              match Hashtbl.find_opt line_numbers derived with
              | Some line ->
                  Format.fprintf fmt "Because %a and %a (%d), %a" explain_incomp ext
                    explain_terms derived.terms line explain_terms incomp.terms
              | None -> (
                  match
                    match derived.cause with
                    | Derived (c1, c2) -> (
                        let* derived, ext =
                          match (is_external c1, is_external c2) with
                          | true, false -> Some (c2, c1)
                          | false, true -> Some (c1, c2)
                          | _ -> None
                        in
                        match Hashtbl.find_opt line_numbers derived with
                        | None -> Some (derived, ext)
                        | _ -> None)
                    | _ -> None
                  with
                  | Some (prior_derived, prior_external) ->
                      Format.fprintf fmt "%a\nAnd because %a and %a, %a" explain_incomp
                        prior_derived explain_incomp prior_external explain_incomp ext
                        explain_terms incomp.terms
                  | _ ->
                      Format.fprintf fmt "%a\nAnd because %a, %a" explain_incomp derived
                        explain_incomp ext explain_terms incomp.terms))
          | true, true ->
              Format.fprintf fmt "Because %a and %a, %a." explain_incomp cause1
                explain_incomp cause2 explain_terms incomp.terms);
          if count_caused incomp root.cause > 1 then
            Format.fprintf fmt " (%d)" (set_line_number incomp)
          else ()
    in
    explain_incomp fmt root
end
