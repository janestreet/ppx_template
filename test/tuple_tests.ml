open! Ppx_template_test_common

[@@@disable_unused_warnings]

[@@@expand_inline
  module%template Delay : sig
    type 'a t = unit -> 'a

    val return : 'a. 'a -> 'a t
  end = struct
    type 'a t = unit -> 'a

    let return x () = x
  end
  [@@mode (p, c) = ((nonportable, uncontended), (portable, contended))]]

module Delay : sig
  type 'a t = unit -> 'a

  val return : 'a. 'a -> 'a t
end = struct
  type 'a t = unit -> 'a

  let return x () = x
end

module Delay__portable : sig
  type 'a t = unit -> 'a

  val return : 'a. 'a -> 'a t
end = struct
  type 'a t = unit -> 'a

  let return x () = x
end

[@@@end]
