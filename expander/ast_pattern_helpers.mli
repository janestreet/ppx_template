open! Stdppx
open! Import
open Language.Untyped

type ('value, 'context, 'a) pattern := unit -> ('context, 'value -> 'a, 'a) Ast_pattern.t

(** A single identifier expression, as in the first identifier in
    [[@@zero_alloc_if_stack a opt]] *)
val ident_expr : (Expression.t loc, expression, _) pattern

(** A single identifier payload, as in [[@exclave_if_stack a]] *)
val single_ident : (Expression.t loc, payload, _) pattern

(** Multiple identifiers, as in [x [@mode m1 m2]] *)
val multiple_idents : (Expression.t loc list, payload, _) pattern

(** A list of bindings, as in
    [[@@mode m1 = (local, global), m2 = (nonportable, portable)]] *)
val bindings : ((Pattern.t * Expression.t Loc.t list) list, payload, _) pattern
