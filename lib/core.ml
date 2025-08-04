open Util

type version = Ast.version
type name = Ast.name
type package = Ast.package
type dependency = package * (name * version list)
type dependencies = dependency list
type repository = package list
type resolver = dependencies -> package -> package list

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

  let check_version_uniqueness (resolution : package list) =
    let names =
      List.fold_left (fun set (n, _) -> StringSet.add n set) StringSet.empty resolution
    in
    StringSet.cardinal names = List.length resolution

  let check_resolution (dependencies : dependencies) ~(root : package)
      ~(resolution : package list) =
    check_root_inclusion ~root ~resolution
    && check_dependency_closure dependencies resolution
    && check_version_uniqueness resolution
end
