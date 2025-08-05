type name = RootName | Name of string
type version = RootVersion | Version of string
type package = name * version
type dependency = package * (name * version list)
type dependencies = (package, name * version list) Hashtbl.t
type available_versions = (name, version) Hashtbl.t
type resolution = package list
type polarity = Pos | Neg
type term = polarity * name * version list

type cause =
  | RootCause
  | NoVersions
  | Dependency of dependency
  | Derived of incompatibility * incompatibility

and incompatibility = { terms : term list; cause : cause }

val pp_resolution : Format.formatter -> (name * version) list -> unit
val set_debug : bool -> unit
val solve : available_versions -> dependencies -> (package list, incompatibility) Result.t
val explain_incompatibility : Format.formatter -> incompatibility -> unit
