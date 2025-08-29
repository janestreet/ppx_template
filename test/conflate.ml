open! Ppx_template_test_common

[@@@disable_unused_warnings]

(* We always conflate modes/modalities for portability, contention, visibiltiy, and access
   axes *)

[@@@expand_inline
  module type Y = sig
    val f : unit -> unit
  end

  module%template.portable [@modality p] P (Y : Y) = struct
    let g = Y.f
  end

  module%template.stateless [@modality v] S (Y : Y) = struct
    let g = Y.f
  end]

module type Y = sig
  val f : unit -> unit
end

module P__portable (Y : sig
    include Y
  end) =
struct
  let g = Y.f
end

module P (Y : sig
    include Y
  end) =
struct
  let g = Y.f
end

module S__stateless (Y : sig
    include Y
  end) =
struct
  let g = Y.f
end

module S__observing (Y : sig
    include Y
  end) =
struct
  let g = Y.f
end

module S (Y : sig
    include Y
  end) =
struct
  let g = Y.f
end

[@@@end]

[@@@expand_inline
  [%%template
  [@@@mode p = (nonportable, portable)]

  type u = { field : unit -> unit } [@@mode p]

  [@@@kind.default k = (value mod p, bits64 mod p)]

  type t]]

include struct
  type u = { field : unit -> unit }

  include struct
    type t__'value_mod_nonportable'
  end [@@ocaml.doc " @inline "]

  include struct
    type t__'bits64_mod_nonportable'
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  type u__portable = { field : unit -> unit }

  include struct
    type t__'value_mod_portable'
  end [@@ocaml.doc " @inline "]

  include struct
    type t__'bits64_mod_portable'
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

[@@@end]

[@@@expand_inline
  [%%template
  [@@@modality s = (stateful, observing, stateless)]

  type u = { field : unit -> unit } [@@modality s]

  [@@@kind.default k = (value mod s, bits64 mod s)]

  type t]]

include struct
  type u = { field : unit -> unit }

  include struct
    type t__'value_mod_stateful'
  end [@@ocaml.doc " @inline "]

  include struct
    type t__'bits64_mod_stateful'
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  type u__observing = { field : unit -> unit }

  include struct
    type t__'value_mod_observing'
  end [@@ocaml.doc " @inline "]

  include struct
    type t__'bits64_mod_observing'
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  type u__stateless = { field : unit -> unit }

  include struct
    type t__'value_mod_stateless'
  end [@@ocaml.doc " @inline "]

  include struct
    type t__'bits64_mod_stateless'
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

[@@@end]
