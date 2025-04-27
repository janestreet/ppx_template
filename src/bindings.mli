open! Stdppx
open! Import

(** Represents the identifier variables introduced by a single binding, along with the set
    of bindings over which they range. Conceptually this is something like a
    [Set.M(Binding).t Map.M(Identifier).t], but stored as lists because the order matters
    for name mangling. *)
type ('id, 'binding) t

module M
    (Identifier : Identifier.S)
    (Binding : Binding.S with type identifier := Identifier.t) : sig
  type nonrec t = (Identifier.t, Binding.t) t
end

(** Returns an error if there are any duplicate mappings. *)
val create
  :  'id Identifier.t
  -> ('binding, 'id, _) Binding.t
  -> ('id * 'binding list) list
  -> (('id, 'binding) t, Sexp.t) result

(** [empty] is equivalent to [create [] |> ok_exn]. *)
val empty : 'id Identifier.t -> ('binding, 'id, _) Binding.t -> ('id, 'binding) t

(** Returns the keys of the association list passed to to [create]. *)
val vars : ('a, _) t -> 'a list

module Instance : sig
  type ('id, 'binding) t = ('id, 'binding) Identifier.map

  module M
      (Identifier : Identifier.S)
      (Binding : Binding.S with type identifier := Identifier.t) : sig
    type nonrec t = (Identifier.t, Binding.t) t
  end
end

(** Enumerate all instances. *)
val instantiate
  :  ('id, 'binding) t
  -> find_identifier:('id -> 'binding option)
  -> ('id, 'binding) Instance.t list
