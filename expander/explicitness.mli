(** [Maybe_explicit] is used by the mangling scheme to decide whether to enumerate all
    manglers, or drop an axis if all manglers for that axis is a default value, or do
    both. *)

type t =
  | Drop_axis_if_all_defaults
  | Explicit
  | Explicit_plus_unmangled

type explicitness := t

val equal : t -> t -> bool
val to_string : t -> string

module With : sig
  type 'a t =
    { explicitness : explicitness
    ; what : 'a
    }

  val what : 'a t -> 'a
  val explicitness : _ t -> explicitness
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val map_result : 'a t -> f:('a -> ('b, 'c) result) -> ('b t, 'c) result
  val ok : ('a, 'b) result t -> ('a t, 'b) result

  module Export : sig
    type 'a _with_explicitness = 'a t =
      { explicitness : explicitness
      ; what : 'a
      }
  end
end

module Each : sig
  type 'a t = private
    { explicit : 'a
    ; explicit_plus_unmangled : 'a
    ; drop_axis_if_all_defaults : 'a
    }

  val create : (explicitness -> 'a) -> 'a t
  val extract_list : 'a t -> 'a list
  val all : 'a t list -> 'a list t
  val combine : 'a option t -> ('a With.t option, [ `multiple ]) result
  val fold_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'b * 'c t
  val map : 'a t -> f:('a -> 'b) -> 'b t
end
