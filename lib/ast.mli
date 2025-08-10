type name = string
type version = string
type package = name * version
type dependency = package * (name * version list) list
type instance = dependency list
type query = (name * version list) list
type packages = package list

val pp : Format.formatter -> instance -> unit
