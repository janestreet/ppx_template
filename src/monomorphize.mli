open! Stdppx
open! Import

module Context : sig
  type t

  val top : t
end

val t : Context.t Ast_traverse.map_with_context
