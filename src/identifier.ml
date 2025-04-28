open! Stdppx
open! Import

type (_, 'a) map = 'a String.Map.t
type _ set = String.Set.t

module type S =
  Identifier_intf.S with type ('a, 'data) map := ('a, 'data) map and type 'a set := 'a set

type 'a t = (module S with type t = 'a)

let make ~defaults =
  (module struct
    include String

    let to_string = Fn.id
    let of_string = Fn.id
    let sexp_of_t = sexp_of_string
    let defaults = String.Set.of_list defaults
  end : S)
;;

module Kind = (val make ~defaults:[ "value" ])
module Mode = (val make ~defaults:[ "global"; "nonportable"; "uncontended" ])
module Modality = (val make ~defaults:[ "local"; "nonportable"; "uncontended" ])

let kind : Kind.t t = (module Kind)
let mode : Mode.t t = (module Mode)
let modality : Modality.t t = (module Modality)
