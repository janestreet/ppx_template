open! Stdppx
open! Import

val to_extension_node : 'a. 'a Attributes.Context.any -> 'a -> Syntax_error.t -> 'a

val to_extension_node_floating
  : 'a.
  'a Attributes.Floating.Context.poly -> loc:location -> Syntax_error.t -> 'a
