open! Stdppx

include module type of struct
  include List
end

val partition_map : 'a t -> f:('a -> ('b, 'c) Either.t) -> 'b t * 'c t
val fold_left : 'elt list -> init:'acc -> f:('acc -> 'elt -> 'acc) -> 'acc
val stable_dedup : 'a list -> cmp:('a -> 'a -> int) -> 'a list

module Or_first_error : sig
  val fold_left
    :  'elt list
    -> init:'acc
    -> f:('acc -> 'elt -> ('acc, 'err) result)
    -> ('acc, 'err) result

  val iter : 'a list -> f:('a -> (unit, 'err) result) -> (unit, 'err) result
  val map : 'a list -> f:('a -> ('b, 'err) result) -> ('b list, 'err) result
  val mapi : 'a list -> f:(int -> 'a -> ('b, 'err) result) -> ('b list, 'err) result
  val filter_map : 'a list -> f:('a -> ('b option, 'err) result) -> ('b list, 'err) result
  val concat_map : 'a list -> f:('a -> ('b list, 'err) result) -> ('b list, 'err) result
end
