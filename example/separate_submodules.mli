open! Core
open! Import
open Each_binding

(* $MDX part-begin=arity1 *)
module type S1 = sig
  type t : any

  val round_up : t -> t
  val round_down : t -> t
  val iround_up_exn : t -> int
  val iround_down_exn : t -> int
end

type ('a : any) module1 = (module S1 with type t = 'a)

val round_up1 : ('a : any). 'a module1 -> 'a -> 'a
val round_down1 : ('a : any). 'a module1 -> 'a -> 'a
val iround_up1_exn : ('a : any). 'a module1 -> 'a -> int
val iround_down1_exn : ('a : any). 'a module1 -> 'a -> int
(* $MDX part-end *)

(* $MDX part-begin=arity0 *)
[%%template:
[@@@kind k = (value, float64)]

module type [@kind k] S0 = sig
  type t : k

  include S1 with type t := t
end

module [@kind k] M : S0 [@kind k] with type t = (float[@kind k])]
(* $MDX part-end *)

(* $MDX part-begin=arity0-of-arity1 *)
[%%template:
[@@@kind k = (value, float64)]

type module0 = ((module S0 with type t = (float[@kind k]))[@kind k]) [@@kind k]

val module0 : (module0[@kind k]) [@@kind k]
val round_up0 : (float[@kind k]) -> (float[@kind k]) [@@kind k]
val round_down0 : (float[@kind k]) -> (float[@kind k]) [@@kind k]
val iround_up0_exn : (float[@kind k]) -> int [@@kind k]
val iround_down0_exn : (float[@kind k]) -> int [@@kind k]]
(* $MDX part-end *)
