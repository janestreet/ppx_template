open! Stdppx

type ('a : value_or_null) t = 'a Nonempty_list_type.Nonempty_list.t =
  | ( :: ) of 'a * 'a list

(** comparison *)

val compare : 'a t -> 'a t -> cmp:('a -> 'a -> int) -> int

(** serialization *)

val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

(** constructors *)

val singleton : 'a -> 'a t
val create : 'a -> 'a list -> 'a t
val cons : 'a -> 'a t -> 'a t

(** accessors *)

val hd : 'a t -> 'a
val tl : 'a t -> 'a list
val length : _ t -> int

(** conversion *)

val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t option
val of_list_exn : here:[%call_pos] -> 'a list -> 'a t

(** other *)

val map : 'a t -> f:('a -> 'b) -> 'b t
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
val sort_uniq : 'a t -> cmp:('a -> 'a -> int) -> 'a t
val stable_dedup : 'a t -> cmp:('a -> 'a -> int) -> 'a t
val concat : 'a t t -> 'a t

(** [product ts] is the cartesian product of all the [t]s in [ts], i.e. a list of all [t]s
    of the form [[t0; t1; ...]] where [t0] is from [ts[0]], [t1] is from [ts[1]], etc. *)
val product : 'a t t -> 'a t t

module Or_first_error : sig
  val fold_left
    :  'elt t
    -> init:'acc
    -> f:('acc -> 'elt -> ('acc, 'err) result)
    -> ('acc, 'err) result

  (** [map t ~f] returns [Ok _] if all [f x] are [Ok _], or else one [Error _]. *)
  val map : 'a t -> f:('a -> ('b, 'err) result) -> ('b t, 'err) result

  val concat_map : 'a t -> f:('a -> ('b t, 'err) result) -> ('b t, 'err) result
end
