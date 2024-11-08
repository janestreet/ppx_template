open! Core
open! Import

(* $MDX part-begin=float *)
[%%template:
[@@@kind k = (value, float64)]

type float : k [@@kind k]

val round_up : (float[@kind k]) -> (float[@kind k]) [@@kind k]
val round_down : (float[@kind k]) -> (float[@kind k]) [@@kind k]
val iround_up_exn : (float[@kind k]) -> int [@@kind k]
val iround_down_exn : (float[@kind k]) -> int [@@kind k]]
(* $MDX part-end *)
