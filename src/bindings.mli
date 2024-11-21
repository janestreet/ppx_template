open! Base
open! Import

(** Represents the identifier variables introduced by a single binding, along with the set
    of bindings over which they range. Conceptually this is something like a
    [Set.M(Binding).t Map.M(Identifier).t], but stored as lists because the order matters
    for name mangling. *)
type ('id, 'binding, 'cmp) t

module M
    (Identifier : Identifier.S)
    (Binding : Binding.S with type identifier := Identifier.t) : sig
  type nonrec t = (Identifier.t, Binding.t, Identifier.comparator_witness) t
end

(** Returns an error if there are any duplicate mappings. *)
val create
  :  ('id, 'cmp) Identifier.t
  -> ('binding, 'id, _) Binding.t
  -> ('id, 'binding list) List.Assoc.t
  -> ('id, 'binding, 'cmp) t Or_error.t

(** [empty] is equivalent to [create [] |> ok_exn]. *)
val empty : ('a, 'cmp) Identifier.t -> ('a, _, 'cmp) t

(** Returns the keys of the association list passed to to [create]. *)
val vars : ('a, _, _) t -> 'a list

module Instance : sig
  type ('id, 'binding, 'cmp) t = ('id, 'binding, 'cmp) Map.t

  module M
      (Identifier : Identifier.S)
      (Binding : Binding.S with type identifier := Identifier.t) : sig
    type nonrec t = (Identifier.t, Binding.t, Identifier.comparator_witness) t
  end
end

(** Enumerate all instances. *)
val instantiate : ('id, 'binding, 'cmp) t -> ('id, 'binding, 'cmp) Instance.t list
