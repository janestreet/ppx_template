open! Stdppx
open! Import

type t = Sexp.t

val to_extension_node : 'a. 'a Attributes.Context.any -> 'a -> t -> 'a

val to_extension_node_floating
  : 'a.
  'a Attributes.Floating.Context.poly -> loc:location -> t -> 'a
