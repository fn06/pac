open Util

type 'a set = 'a list
type version = Ast.version
type name = Ast.name
type package = Ast.package
type dependency = package * (name * version set)
type dependencies = dependency set
type repository = package set
type query = package set
type resolver_resolution = dependencies -> query -> package set

let convert_dependency (pkg, targets) : dependencies =
  List.map (fun t -> (pkg, t)) targets

let of_ast_expression (deps : Ast.expression) : dependencies =
  List.flatten @@ List.map convert_dependency deps

let repository_from_ast (deps : Ast.expression) : repository =
  List.fold_left
    (fun acc (pkg, _targets) -> if List.mem pkg acc then acc else pkg :: acc)
    [] deps

let to_ast_dependency ((pkg, (name, versions)) : dependency) : Ast.dependency =
  (pkg, [ (name, versions) ])

let to_ast_expression (deps : dependencies) : Ast.expression =
  List.map to_ast_dependency deps

module Resolution = struct
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
