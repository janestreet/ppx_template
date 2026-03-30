open! Stdppx
open! Import

module Definitions = struct
  (** A subset of [Ast_traverse.map_with_context]. We can expose more as needed, however
      not all methods from that class make good entry points to our implementation.

      Be sure to check the implementation before exposing more entry points here. *)
  class type ['ctx] t = object
    method core_type : 'ctx -> core_type -> core_type
    method expression : 'ctx -> expression -> expression
    method module_expr : 'ctx -> module_expr -> module_expr
    method module_type : 'ctx -> module_type -> module_type
    method pattern : 'ctx -> pattern -> pattern
    method signature : 'ctx -> signature -> signature
    method signature_item_inline : 'ctx -> signature_item -> signature_item list
    method signature_items : 'ctx -> signature_item list -> signature_item list
    method structure : 'ctx -> structure -> structure
    method structure_item_inline : 'ctx -> structure_item -> structure_item list
  end
end

module type Monomorphize = sig
  include module type of struct
    include Definitions
  end

  module Context : sig
    type t

    val top : t
  end

  (** Perform the main [ppx_template] expansion over the ast. See [t_inline] below for
      explanation of [no_inline] suffix. *)
  val t : Context.t t
end
