open! Stdppx
open! Import
open Language

(** A [('a, 'node) pat] constructs an [Ast_pattern] that parse ['node]s and produce ['a]s.
    This is used to get around the value restriction when the same pattern needs to be
    used multiple times. *)
type ('a, 'node) pat = private { pat : 'b. unit -> ('node, 'a -> 'b, 'b) Ast_pattern.t }
[@@unboxed]

(** Parses OCaml expressions like [a b c] as function applications, returning a list of
    the expressions (e.g. [[ a; b; c ]]). *)
val one_or_many_as_list
  :  ('a, expression) pat
  -> (expression, 'a list -> 'b, 'b) Ast_pattern.t

(** Parses OCaml identifier expressions as an [Identifier.t], associating it with the
    given [Type.t]. *)
val ident : 'a Type.t -> ('a Identifier.t, expression) pat

(** Parses OCaml identifier expression as an [Expression.t]. *)
val ident_expr : 'a Type.t -> ('a Expression.t Loc.t, expression) pat

(** Parses OCaml identifier expressions as a [Pattern.t]. *)
val ident_pattern : 'a Type.t -> ('a Pattern.t, expression) pat

(** Parses OCaml expressions as kind expressions, like [bits32] or [value mod portable]. *)
val kind_expr : (Type.kind Expression.t Loc.t, expression) pat

(** Parses OCaml expressions like [alloc @ mode] as a tuple of an [alloc] and [mode]
    pattern. *)
val alloc_pattern : ((Type.alloc * Type.mode) Pattern.t, expression) pat

(** Parses OCaml expressions like [alloc] or [alloc @ mode] as a tuple of an [alloc] and
    [mode] expression. *)
val alloc_expr : ((Type.alloc * Type.mode) Expression.t Loc.t, expression) pat

(** Parses a [payload] of the form [[@attr l = (a, b), m = (c, d)]] as
    [[ "l", [ "a"; "b" ]; "m", [ "c"; "d" ] ]], or of the form [[@attr a b]] as
    [[ "a", [ "a" ]; "b", [ "b" ]]]. *)
val binding_poly
  :  pattern_pat:('a Pattern.t, expression) pat
  -> expression_pat:('a Expression.t Loc.t, expression) pat
  -> mangle:('a Value.t -> 'mangle Type.basic Value.t)
  -> mangle_type:'mangle Type.basic Type.t
  -> (payload, Attributes_intf.Definitions.Poly.t -> 'c, 'c) Ast_pattern.t

(** Parses a [payload] of the form [[@attr a b c]] as [[ "a"; "b"; "c" ]]. **)
val binding_mono
  :  ('a Type.basic Expression.t Loc.t, expression) pat
  -> (payload, Expression.Basic.packed Loc.t list -> 'b, 'b) Ast_pattern.t
