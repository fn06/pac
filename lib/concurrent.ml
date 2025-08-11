open Core

type granularity = version -> version

let encode_name n g = Name (Format.asprintf "<%a-%a>" pp_name n pp_version g)

let encode_dep g ((n, v), (m, vs)) =
  let granular =
    List.fold_left (fun set v -> VersionSet.add (g v) set) VersionSet.empty vs
  in
  if VersionSet.cardinal granular <= 1 then
    (* Direct case *)
    let w = VersionSet.choose granular in
    [ ((encode_name n (g v), v), (encode_name m w, vs)) ]
  else
    (* Split case *)
    let intermediate =
      Name (Format.asprintf "<%a-%a-%a>" pp_name n pp_version v pp_name m)
    in
    let gvs = VersionSet.to_list granular in
    let dependant = [ ((encode_name n (g v), v), (intermediate, gvs)) ] in
    let intermediates =
      List.concat_map
        (fun w ->
          let vs_w = List.filter (fun v -> g v = w) vs in
          [ ((intermediate, w), (encode_name m w, vs_w)) ])
        gvs
    in
    dependant @ intermediates

let encode g core =
  let repo, deps = core in
  let reduced_repo =
    List.map (fun (n, v) -> (encode_name n (g v), v)) repo
    @ List.fold_left
        (fun acc ((n, v), (m, vs)) ->
          let granular =
            List.fold_left (fun set v -> VersionSet.add (g v) set) VersionSet.empty vs
          in
          if VersionSet.cardinal granular <= 1 then acc
          else
            let intermediate =
              Name (Format.asprintf "<%a-%a-%a>" pp_name n pp_version v pp_name m)
            in
            let gvs = VersionSet.to_list granular in
            List.map (fun v -> (intermediate, v)) gvs @ acc)
        [] deps
  in
  let reduced_deps = deps |> List.concat_map (encode_dep g) in
  (reduced_repo, reduced_deps)

module Resolution = struct
  let check_root_inclusion = Core.Resolution.check_root_inclusion

  let check_dependency_closure dependencies resolution =
    List.for_all
      (fun (p, (m, vs)) ->
        match List.mem p resolution with
        | false -> true
        | true -> (
            match List.find_opt (fun (o, v) -> o = m && List.mem v vs) resolution with
            | Some _ -> true
            | None -> false))
      dependencies

  let check_version_granularity g resolution =
    let name_map = Hashtbl.create 0 in
    List.iter
      (fun (name, version) ->
        let versions = try Hashtbl.find name_map name with Not_found -> [] in
        Hashtbl.add name_map name (version :: versions))
      resolution;
    Hashtbl.fold
      (fun _ versions acc ->
        acc
        &&
        let len = List.length versions in
        if len <= 1 then true
        else
          let granular_set =
            List.fold_left
              (fun set v -> VersionSet.add (g v) set)
              VersionSet.empty versions
          in
          VersionSet.cardinal granular_set = len)
      name_map true

  let check_concurrent_resolution g dependencies resolution =
    check_root_inclusion resolution
    && check_dependency_closure dependencies resolution
    && check_version_granularity g resolution
end
