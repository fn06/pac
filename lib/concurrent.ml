open Util
open Core

type granularity = version -> version

let encode_name (n : name) (g : version) : name = n ^ "-" ^ g

let encode_dep (g : granularity) (((n, v), (m, vs)) : dependency) : dependencies =
  let granular =
    List.fold_left (fun set v -> StringSet.add (g v) set) StringSet.empty vs
  in
  (* Direct case *)
  if StringSet.cardinal granular <= 1 then
    let w = StringSet.choose granular in
    [ ((encode_name n (g v), v), (encode_name m w, vs)) ] (* Split case *)
  else
    let intermediate = Printf.sprintf "i-%s-%s-%s" n v m in
    let gvs = StringSet.to_list granular in
    let dependant = [ ((encode_name n (g v), v), (intermediate, gvs)) ] in
    let intermediates =
      List.concat_map
        (fun w ->
          let vs_w = List.filter (fun v -> g v = w) vs in
          [ ((intermediate, w), (encode_name m w, vs_w)) ])
        gvs
    in
    dependant @ intermediates

let encode_dependencies (g : granularity) (dependencies : dependencies) : dependencies =
  dependencies |> List.concat_map (encode_dep g)

module Resolution = struct
  let check_root_inclusion ~(root : package) ~(resolution : package list) =
    List.mem root resolution

  let check_dependency_closure (dependencies : dependencies) (resolution : package list) =
    List.for_all
      (fun (p, (m, vs)) ->
        match List.mem p resolution with
        | false -> true
        | true -> (
            match
              List.find_opt (fun (o, v) -> String.equal o m && List.mem v vs) resolution
            with
            | Some _ -> true
            | None -> false))
      dependencies

  let check_version_granularity (g : granularity) (resolution : package list) =
    let name_map =
      List.fold_left
        (fun map (name, version) ->
          let versions = try StringMap.find name map with Not_found -> [] in
          StringMap.add name (version :: versions) map)
        StringMap.empty resolution
    in
    StringMap.for_all
      (fun _ versions ->
        let len = List.length versions in
        if len <= 1 then true
        else
          let granular_set =
            List.fold_left (fun set v -> StringSet.add (g v) set) StringSet.empty versions
          in
          StringSet.cardinal granular_set = len)
      name_map

  let check_concurrent_resolution (g : granularity) (dependencies : dependencies)
      ~(root : package) ~(resolution : package list) =
    check_root_inclusion ~root ~resolution
    && check_dependency_closure dependencies resolution
    && check_version_granularity g resolution
end
