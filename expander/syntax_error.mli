open! Stdppx

type t

val createf
  :  loc:Ppxlib.Location.t
  -> ('a, Stdlib.Format.formatter, unit, t) format4
  -> 'a

val of_location_errors : Ppxlib.Location.Error.t NonEmptyList.t -> t
val combine : t Nonempty_list.t -> t
val to_extension : t -> Ppxlib.extension
