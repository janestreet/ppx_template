open! Core

[@@@disable_unused_warnings]

(* We always conflate modes/modalities for portability and contention axes *)

[@@@expand_inline
  module type Y = sig
    val f : unit -> unit
  end

  module%template.portable [@modality p] M (Y : Y) = struct
    let g @ p = Y.f
  end]

module type Y = sig
  val f : unit -> unit
end

include struct
  module M__portable (Y : sig
    @@ portable
      include Y
    end) =
  struct
    let g @ portable = Y.f
  end

  module M (Y : sig
    @@ nonportable
      include Y
    end) =
  struct
    let g @ nonportable = Y.f
  end
end

[@@@end]

[@@@expand_inline
  [%%template
  [@@@mode p = (nonportable, portable)]

  type u = { field : unit -> unit @@ p } [@@mode p]

  [@@@kind.default k = (value mod p, bits64 mod p)]

  type t : k]]

include struct
  type u = { field : unit -> unit @@ nonportable }

  include struct
    type t__'value_mod_nonportable' : value mod nonportable
  end

  include struct
    type t__'bits64_mod_nonportable' : bits64 mod nonportable
  end
end

include struct
  type u__portable = { field : unit -> unit @@ portable }

  include struct
    type t__'value_mod_portable' : value mod portable
  end

  include struct
    type t__'bits64_mod_portable' : bits64 mod portable
  end
end

[@@@end]
