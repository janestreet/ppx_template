open! Core

[@@@disable_unused_warnings]

[@@@expand_inline
  [%%template
  [@@@mode m = (local, global)]

  module T : sig
    type t = { x : string @ m -> string @ m @@ m } [@@conflate_mode_as_modality m]
  end = struct
    type t = { x : string @ m -> string @ m @@ m } [@@conflate_mode_as_modality m]
  end]]

include struct
  module T : sig
    type t = { x : local_ string -> local_ string @@ local }
  end = struct
    type t = { x : local_ string -> local_ string @@ local }
  end
end

include struct
  module T : sig
    type t = { global_ x : string @ global -> string @ global }
  end = struct
    type t = { global_ x : string @ global -> string @ global }
  end
end

[@@@end]

[@@@expand_inline
  [%%template
    let f (x @ m) = x [@@modality m = (local, global)] [@@conflate_modality_as_mode m]]]

let f (local_ x) = x
and f__global (x @ global) = x

[@@@end]

[@@@expand_inline
  module type X = sig
    val f : unit -> unit
  end

  module%template.portable [@modality p] [@conflate_modality_as_mode p] M (X : X) = struct
    let g @ p = X.f
  end]

module type X = sig
  val f : unit -> unit
end

include struct
  module M__portable (X : sig
    @@ portable
      include X
    end) =
  struct
    let g @ portable = X.f
  end

  module M (X : sig
    @@ nonportable
      include X
    end) =
  struct
    let g @ nonportable = X.f
  end
end

[@@@end]
