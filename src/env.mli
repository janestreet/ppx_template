open! Base
open! Import

(** A map from the kind and mode variables introduced by this PPX to their current
    bindings.

    You could imagine having these be two different types, such that this is e.g. a
    [Jkind.t Map.M(Lvar).t]. But we need this PPX precisely because we have no syntax for
    kind variables; the only annotation we can get out of the AST is a [Jkind.t], and
    introducing a second [Lvar.t] type mostly results in a lot of conversion between the
    two, for little benefit. Plus an arbitrary annotation in the AST could be either a
    kind variable or a constant, so "lvar" is kind of a misnomer. *)
type t

val create
  :  kinds:Bindings.Instance.M(Identifier.Kind)(Binding.Kind).t
  -> modes:Bindings.Instance.M(Identifier.Mode)(Binding.Mode).t
  -> t

val empty : t

type ('id, 'binding) find := t -> 'id -> 'binding option
type ('id, 'binding) find_exn := t -> 'id -> 'binding

(** Look up the current binding for this identifier. *)
val find_kind : (Identifier.Kind.t, Binding.Kind.t) find

(** As [find_kind], but for modes. *)
val find_mode : (Identifier.Mode.t, Binding.Mode.t) find

val find_kind_exn : (Identifier.Kind.t, Binding.Kind.t) find_exn
val find_mode_exn : (Identifier.Mode.t, Binding.Mode.t) find_exn

(** [lookup_and_set_all ~current_env ~uninterpreted_env] looks up any kinds or modes in
    [uninterpreted_env] bound in [current_env], and then merges the two environments,
    resolving conflicts by preferring [uninterpreted_env]. *)
val lookup_and_set_all : current_env:t -> uninterpreted_env:t -> t
