open! Stdppx
open! Import
open Language
open Typed

module type Env = sig
  type t

  (** An [Env.t] populated with initial bindings for [heap] and [stack] *)
  val initial : t

  val find : t -> 'a Identifier.t -> 'a Value.t option
  val find_expanding_sets : t -> 'a Identifier.t -> 'a Value.t Nonempty_list.t option

  (** [bind env ~loc ~is_set pat value] adds a new binding to [pat] in [env]. [value] is
      used for the [preserve_atoms] entry, and its fully expanded version for the
      [expand_atoms_bound_to_sets] entry.

      Produces an [Error] if binding against this pattern would shadow an identifier from
      a different namespace, or if evaluating value while expanding identifiers produces
      an error. *)
  val bind
    :  t
    -> loc:location
    -> is_set:bool
    -> 'a Pattern.t
    -> 'a Value.t
    -> (t, Syntax_error.t) result

  (** [bind_set env ~loc ident values] adds a new set binding to [ident] in [env]. [ident]
      is used as the [preserve_atoms] entry, and [values] as the
      [expand_atoms_bound_to_sets] entry. *)
  val bind_set
    :  t
    -> loc:location
    -> 'a Type.non_tuple Pattern.t
    -> 'a Type.non_tuple Value.t Nonempty_list.t
    -> (t, Syntax_error.t) result

  (** Evaluate an expression in the given environment. Unbound [Expression.Identifier]s
      are evaluated as an equivalent [Value.Identifier] under the assumption that the
      identifier will be interpreted by the OCaml compiler; if it is not, we let the
      compiler report the error to the user. *)

  (** Evaluate a singleton expression without expanding identifiers bound to sets,
      producing a single value. *)
  val eval_singleton
    :  t
    -> ('a, Expression.singleton) Expression.t Loc.t
    -> ('a Value.t, Syntax_error.t) result

  (** Evaluate an expression that may contain unions, treating identifiers as determined
      by [lookup]. Always produces a set of values. *)
  val eval
    :  t
    -> Attribute_handler.lookup
    -> ('a, Expression.set) Expression.t Loc.t
    -> ('a Value.t Nonempty_list.t, Syntax_error.t) result

  val instantiate
    :  t
    -> Attribute_handler.Poly.t Explicitness.With.t list
    -> ( (t * Value.Basic.packed list Explicitness.With.t Axis.Map.t) Nonempty_list.t
         , Syntax_error.t )
         result
end
