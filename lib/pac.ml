module Ast = Ast
module Parser = Parser
module Lexer = Lexer
module Pubgrub = Pubgrub

module Core = struct
  type 'a set = 'a list
  type version = Ast.version
  type name = Ast.name
  type package = Ast.package
  type dependency = package * (name * version set)
  type dependencies = dependency set
  type repository = package set
  type query = package set
  type resolver_resolution = dependencies -> query -> package set
end

let convert_dependency (pkg, targets) : Core.dependencies =
  List.map (fun t -> (pkg, t)) targets

let of_ast_expression (deps : Ast.expression) : Core.dependencies =
  List.flatten @@ List.map convert_dependency deps

let repository_from_ast (deps : Ast.expression) : Core.repository =
  List.fold_left
    (fun acc (pkg, _targets) -> if List.mem pkg acc then acc else pkg :: acc)
    [] deps

let to_ast_dependency ((pkg, (name, versions)) : Core.dependency) :
    Ast.dependency =
  (pkg, [ (name, versions) ])

let to_ast_expression (deps : Core.dependencies) : Ast.expression =
  List.map to_ast_dependency deps

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

module Resolution = struct
  open Core

  let check_query_inclusion ~(query : query) ~(resolution : package set) =
    List.for_all (fun q -> List.mem q resolution) query

  let check_dependency_closure (dependencies : dependencies)
      (resolution : package set) =
    List.for_all
      (fun (p, (m, vs)) ->
        match List.mem p resolution with
        | false -> true
        | true -> (
            match
              List.find_opt
                (fun (o, v) -> String.equal o m && List.mem v vs)
                resolution
            with
            | Some _ -> true
            | None -> false))
      dependencies

  let check_version_uniqueness (resolution : package set) =
    let names =
      List.fold_left
        (fun set (n, _) -> StringSet.add n set)
        StringSet.empty resolution
    in
    StringSet.cardinal names = List.length resolution

  let check_resolution (dependencies : dependencies) ~(query : query)
      ~(resolution : package set) =
    check_query_inclusion ~query ~resolution
    && check_dependency_closure dependencies resolution
    && check_version_uniqueness resolution
end

module Concurrent = struct
  open Core

  type granularity = version -> version

  type concurrent_resolution =
    granularity -> dependencies -> query -> package set

  let encode_name (n : name) (g : version) : name = n ^ "-" ^ g

  let encode_query (g : granularity) (q : query) : query =
    List.map (fun (n, v) -> (encode_name n (g v), v)) q

  let encode_dep (g : granularity) (((n, v), (m, vs)) : dependency) :
      dependencies =
    let granular =
      List.fold_left (fun set v -> StringSet.add (g v) set) StringSet.empty vs
    in
    (* Direct case *)
    if StringSet.cardinal granular <= 1 then
      let w = StringSet.choose granular in
      [ ((encode_name n (g v), v), (encode_name m w, vs)) ]
    (* Split case *)
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

  let encode_dependencies (g : granularity) (dependencies : dependencies) :
      dependencies =
    dependencies |> List.concat_map (encode_dep g)
end

module ConcurrentResolution = struct
  open Core

  let check_query_inclusion ~(query : query) ~(resolution : package set) =
    List.for_all (fun q -> List.mem q resolution) query

  let check_dependency_closure (dependencies : dependencies)
      (resolution : package set) =
    List.for_all
      (fun (p, (m, vs)) ->
        match List.mem p resolution with
        | false -> true
        | true -> (
            match
              List.find_opt
                (fun (o, v) -> String.equal o m && List.mem v vs)
                resolution
            with
            | Some _ -> true
            | None -> false))
      dependencies

  let check_version_granularity (g : Concurrent.granularity)
      (resolution : package set) =
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
            List.fold_left
              (fun set v -> StringSet.add (g v) set)
              StringSet.empty versions
          in
          StringSet.cardinal granular_set = len)
      name_map

  let check_concurrent_resolution (g : Concurrent.granularity)
      (dependencies : dependencies) ~(query : query) ~(resolution : package set)
      =
    check_query_inclusion ~query ~resolution
    && check_dependency_closure dependencies resolution
    && check_version_granularity g resolution
end
