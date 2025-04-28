open! Core

[@@@disable_unused_warnings]

[@@@expand_inline
  [%%template
  [@@@mode m = (local, global)]

  module T : sig
    type t = { x : string -> string } [@@conflate_mode_as_modality m]
  end = struct
    type t = { x : string -> string } [@@conflate_mode_as_modality m]
  end]]

include struct
  module T : sig
    type t = { x : string -> string }
  end = struct
    type t = { x : string -> string }
  end
end

include struct
  module T : sig
    type t = { x : string -> string [@globalized] }
  end = struct
    type t = { x : string -> string [@globalized] }
  end
end

[@@@end]

[@@@expand_inline
  [%%template
    let f x = x [@@modality m = (local, global)] [@@conflate_modality_as_mode m]]]

let f x = x
and f__global x = x

[@@@end]

[@@@expand_inline
  module type X = sig
    val f : unit -> unit
  end

  module%template.portable [@modality p] [@conflate_modality_as_mode p] M (X : X) = struct
    let g = X.f
  end]

module type X = sig
  val f : unit -> unit
end

include struct
  module M__portable (X : sig
      include X
    end) =
  struct
    let g = X.f
  end

  module M (X : sig
      include X
    end) =
  struct
    let g = X.f
  end
end

[@@@end]
