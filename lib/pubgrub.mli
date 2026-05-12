module type NAME = sig
  type t

  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

module type VERSION = sig
  type t

  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

val set_debug : bool -> unit

module Make (N : NAME) (V : VERSION) : sig
  type incompatibility
  type repository = (N.t * V.t) list
  type dependencies = ((N.t * V.t) * (N.t * V.t list)) list
  type query = (N.t * V.t list) list

  val resolve :
    repository ->
    dependencies ->
    query ->
    ((N.t * V.t) list, incompatibility) Result.t

  val explain_incompatibility : Format.formatter -> incompatibility -> unit
end
