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

module _ = struct
  [@@@expand_inline
    [%%template
    [@@@mode p = (nonportable, portable)]

    type u = { field : unit -> unit } [@@mode p]

    [@@@kind.default k = (value mod p, bits64 mod p)]

    type t]]

  type u = { field : unit -> unit }
  type t__'value_mod_nonportable'
  type t__'bits64_mod_nonportable'
  type u__portable = { field : unit -> unit }
  type t__'value_mod_portable'
  type t__'bits64_mod_portable'

  [@@@end]
end

module _ = struct
  [@@@expand_inline
    [%%template
    [@@@modality s = (stateful, observing, stateless)]

    type u = { field : unit -> unit } [@@modality s]

    [@@@kind.default k = (value mod s, bits64 mod s)]

    type t]]

  type u = { field : unit -> unit }
  type t__'value_mod_stateful'
  type t__'bits64_mod_stateful'
  type u__observing = { field : unit -> unit }
  type t__'value_mod_observing'
  type t__'bits64_mod_observing'
  type u__stateless = { field : unit -> unit }
  type t__'value_mod_stateless'
  type t__'bits64_mod_stateless'

  [@@@end]
end
