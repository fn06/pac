type incompatibility

val set_debug : bool -> unit

val resolve :
  Core.repository -> Core.dependencies -> (Core.package list, incompatibility) Result.t

val explain_incompatibility : Format.formatter -> incompatibility -> unit
