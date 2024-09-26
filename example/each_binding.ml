open! Core
open! Import

(* $MDX part-begin=float *)
[%%template
[@@@kind k = (value, float64)]

open Float [@kind k]

type float = t [@@kind k]

let[@kind k] round_up = round_up
let[@kind k] round_down = round_up
let[@kind k] iround_up_exn = iround_up_exn
let[@kind k] iround_down_exn = iround_down_exn]
(* $MDX part-end *)
