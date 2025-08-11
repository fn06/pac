open Core

type granularity = version -> version

val encode_name : name -> version -> name
val encode_dep : granularity -> dependency -> dependencies
val encode : granularity -> instance -> instance

module Resolution : sig
  val check_root_inclusion : repository -> bool
  val check_dependency_closure : dependencies -> repository -> bool
  val check_version_granularity : granularity -> repository -> bool
  val check_concurrent_resolution : granularity -> dependencies -> repository -> bool
end
