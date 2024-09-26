open! Base
open! Import

(** Represents the identifier variables introduced by a single binding, along with the set
    of values over which they range. Conceptually this is something like a
    [Set.M(Id).t Map.M(Id).t], but stored as lists because the order matters for name
    mangling. *)
type ('a, 'cmp) t

module M (Identifier : Identifier.S) : sig
  type nonrec t = (Identifier.t, Identifier.comparator_witness) t
end

(** Returns an error if there are any duplicate mappings. *)
val create
  :  ('a, 'cmp) Identifier.t
  -> ('a, 'a list) List.Assoc.t
  -> ('a, 'cmp) t Or_error.t

(** [empty] is equivalent to [create [] |> ok_exn]. *)
val empty : ('a, 'cmp) Identifier.t -> ('a, 'cmp) t

(** Returns the keys of the association list passed to to [create]. *)
val vars : ('a, _) t -> 'a list

module Instance : sig
  type ('a, 'cmp) t = ('a, 'a, 'cmp) Map.t

  module M (Identifier : Identifier.S) : sig
    type nonrec t = (Identifier.t, Identifier.comparator_witness) t
  end
end

(** Enumerate all instances. *)
val instantiate : ('a, 'cmp) t -> ('a, 'cmp) Instance.t list
