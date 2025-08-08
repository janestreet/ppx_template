open! Stdppx
open! Import

module Context : sig
  type t

  val top : t
end

(** Perform the main [ppx_template] expansion over the ast. See [t_inline] below for
    explanation of [no_inline] suffix. *)
val t_no_inline : Context.t Ast_traverse.map_with_context

(** Inserts [[%%template.inline]] nodes in order to reduce the number of necessary
    [include struct] and [include sig] items created. This is mostly useful for when the
    templated code is inserted into the source code in order to reduce clutter.

    NOTE: Producing code via [t_inline] requires depending on [ppx_template] itself. Most
    external cases will want to use [t_no_inline] to avoid dependency issues. *)
val t_inline : Context.t Ast_traverse.map_with_context
