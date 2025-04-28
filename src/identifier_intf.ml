open! Stdppx
open! Import

module type S = sig
  type t
  type ('a, !+'data) map
  type 'elt set

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val sexp_of_t : t -> Sexp.t
  val to_string : t -> string
  val of_string : string -> t

  module Map : Map.S with type key = t and type 'data t = (t, 'data) map
  module Set : Set.S with type elt = t and type t = t set

  val defaults : Set.t
end

module type Identifier = sig
  type ('a, !+'data) map
  type 'a set

  module type S =
    S with type ('key, 'data) map := ('key, 'data) map and type 'key set := 'key set

  type 'a t = (module S with type t = 'a)

  module Kind : S
  module Mode : S
  module Modality : S

  val kind : Kind.t t
  val mode : Mode.t t
  val modality : Modality.t t
end
