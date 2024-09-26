open! Base
open! Import

module Suffix : sig
  type t

  val create
    :  env:Env.t
    -> kinds:Identifier.Kind.t list
    -> modes:Identifier.Mode.t list
    -> t
end

(** We piggyback on [Ast_traverse] because it gives us a lot of AST traversal code for
    free. However, only the methods we currently need are implemented - if adding support
    for new kinds of bindings in [Monomorphize], be sure to update [Mangle].

    An alternative implementation for this could be
    [Suffix.t -> location Ast_traverse.map_with_context], with [Suffix.t = string list],
    rather than [string list loc] as it secretly is now. That is, we don't modify the
    [string list] part of the suffix as we traverse. The [location] is updated as we
    traverse to the nearest enclosing location, to be used in error reporting.

    However, passing the [string list] via [map_with_context] allows us to cheaply reuse a
    global singleton [Mangle] object for every identifier we need to mangle, calling pure
    functions, rather than creating a fresh object with many methods and potentially some
    closures each time. *)
val t : Suffix.t Ast_traverse.map_with_context
