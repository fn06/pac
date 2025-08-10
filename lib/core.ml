type name = RootName | Name of string
type version = RootVersion | Version of string
type package = name * version
type repository = package list
type dependency = package * (name * version list)
type dependencies = dependency list
type resolution = package list
type instance = repository * dependencies

module NameSet = Set.Make (struct
  type t = name

  let compare = compare
end)

module VersionSet = Set.Make (struct
  type t = version

  let compare = compare
end)

let pp_name fmt = function
  | RootName -> Format.pp_print_string fmt "Root"
  | Name n -> Format.pp_print_string fmt n

let pp_version fmt = function
  | RootVersion -> Format.pp_print_string fmt "Root"
  | Version n -> Format.pp_print_string fmt n

let pp_versions fmt vs =
  Format.fprintf fmt "(%a)"
    Format.(
      pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
        (fun fmt v -> fprintf fmt "%a" pp_version v))
    vs

let pp_package fmt = function
  | RootName, RootVersion -> Format.fprintf fmt "root"
  | n, v -> Format.fprintf fmt "%a %a" pp_name n pp_version v

let pp_dependency fmt (p, (n, vs)) =
  Format.fprintf fmt "%a -> %a %a" pp_package p pp_name n pp_versions vs

let pp_packages =
  Format.(
    pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
      (fun fmt (n, v) -> fprintf fmt "%a %a" pp_name n pp_version v))

let of_ast ast =
  List.fold_left
    (fun (repo, deps) ((n, v), targets) ->
      let pkg = (Name n, Version v) in
      ( pkg :: repo,
        List.map
          (fun (m, vs) -> (pkg, (Name m, List.map (fun v -> Version v) vs)))
          targets
        @ deps ))
    ([], []) ast

let to_ast instance =
  let repo, deps = instance in
  List.filter_map
    (function
      | (Name n, Version v) as pkg ->
          Some
            ( (n, v),
              List.filter_map
                (function
                  | depender, (Name m, vs) ->
                      if depender = pkg then
                        Some
                          ( m,
                            List.filter_map
                              (function Version v -> Some v | _ -> None)
                              vs )
                      else None
                  | _ -> None)
                deps )
      | _ -> None)
    repo

module Resolution = struct
  let check_root_inclusion resolution = List.mem (RootName, RootVersion) resolution

  let check_dependency_closure dependencies resolution =
    List.for_all
      (fun (p, (m, vs)) ->
        match List.mem p resolution with
        | false -> true
        | true -> (
            match
              List.find_opt (function o, v -> o = m && List.mem v vs) resolution
            with
            | Some _ -> true
            | None -> false))
      dependencies

  let check_version_uniqueness resolution =
    let names =
      List.fold_left (fun set (n, _) -> NameSet.add n set) NameSet.empty resolution
    in
    NameSet.cardinal names = List.length resolution

  let check_resolution dependencies resolution =
    check_root_inclusion resolution
    && check_dependency_closure dependencies resolution
    && check_version_uniqueness resolution
end
