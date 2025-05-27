open! Core

[@@@disable_unused_warnings]

(* We always conflate modes/modalities for portability and contention axes *)

[@@@expand_inline
  module type Y = sig
    val f : unit -> unit
  end

  module%template.portable [@modality p] M (Y : Y) = struct
    let g = Y.f
  end]

module type Y = sig
  val f : unit -> unit
end

include struct
  module M__portable (Y : sig
      include Y
    end) =
  struct
    let g = Y.f
  end

  module M (Y : sig
      include Y
    end) =
  struct
    let g = Y.f
  end
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
  end

  include struct
    type t__'bits64_mod_nonportable'
  end
end

include struct
  type u__portable = { field : unit -> unit }

  include struct
    type t__'value_mod_portable'
  end

  include struct
    type t__'bits64_mod_portable'
  end
end

[@@@end]
