type name = RootName | Name of string
type version = RootVersion | Version of string
type package = name * version
type repository = package list
type dependency = package * (name * version list)
type dependencies = dependency list
type resolution = package list
type instance = repository * dependencies

module NameSet : Set.S with type elt = name
module VersionSet : Set.S with type elt = version

val pp_name : Format.formatter -> name -> unit
val pp_version : Format.formatter -> version -> unit
val pp_versions : Format.formatter -> version list -> unit
val pp_package : Format.formatter -> package -> unit
val pp_packages : Format.formatter -> package list -> unit
val pp_dependency : Format.formatter -> dependency -> unit
val of_ast : Ast.instance -> instance
val to_ast : instance -> Ast.instance

module Resolution : sig
  val check_root_inclusion : (name * version) list -> bool
  val check_dependency_closure : dependencies -> resolution -> bool
  val check_version_uniqueness : resolution -> bool
  val check_resolution : dependencies -> resolution -> bool
end
