type%template 'a t =
  { x : int
  ; a : 'a
  }
[@@kind k = base]

[@@@expand_inline:
  val%template unbox : 'a. ('a t[@kind k]) -> ('a t[@kind k]) [@@kind k = base]]

[@@@ocaml.text "/*"]

val unbox__bits64 : 'a. 'a t__bits64 -> 'a t__bits64
val unbox__bits32 : 'a. 'a t__bits32 -> 'a t__bits32
val unbox__word : 'a. 'a t__word -> 'a t__word
val unbox__float64 : 'a. 'a t__float64 -> 'a t__float64
val unbox__float32 : 'a. 'a t__float32 -> 'a t__float32

[@@@ocaml.text "/*"]

val unbox : 'a. 'a t -> 'a t

[@@@end]
