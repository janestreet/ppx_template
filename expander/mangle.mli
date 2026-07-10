open! Stdppx
open! Import
open Language.Typed

module Suffix : sig
  type t

  val create : Value.Basic.packed list Explicitness.With.t Axis.Map.t -> t
end

module Outcome : sig
  type t

  val did_mangle : t -> bool
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
val t : (Suffix.t, Outcome.t) Ast_traverse.lift_map_with_context

(** Apply name mangling to the item, using the given attribute expressions and
    environment. *)
val mangle
  :  'a Attribute_handler.Context.mono
  -> 'a
  -> Expression.Basic.packed Loc.t list Explicitness.With.t Axis.Map.t
  -> env:Env.t
  -> 'a

(** This is useful for ppxes outside of ppx_template, when they want to generate names
    that are mangled based on jkind annotations and don't want to generate attribute and
    extension annotations to get that done.

    This is only for use by other ppxs: code in ppx_template should always use [Mangle.t]
    instead. *)
val suffix_for_manual_mangling
  :  ?modes:Explicitness.t * modes
  -> ?kinds:Explicitness.t * jkind_annotation list
  -> unit
  -> (string, Syntax_error.t) result
[@@alert
  for_specific_ppx_uses
    "[suffix_for_manual_mangling] is intended to allow other ppxs to produce names that \
     interoperate with [ppx_template]. Speak to a [ppx_template] reviewer if you believe \
     you have a reason to use [suffix_for_manual_mangling] in your ppx."]
