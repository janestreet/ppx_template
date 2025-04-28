open! Core

(* Define [Applicative] with a [Make] functor polymorphic over the portability
   of the functions. *)
module Applicative = struct
  module type Arg = sig
    type 'a t

    val return : 'a -> 'a t
    val apply : 'a t -> ('a -> 'b) t -> 'b t
  end

  module type S = sig
    include Arg

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module%template.portable Make (Arg : Arg) : S with type 'a t := 'a Arg.t = struct
    include Arg

    let map t ~f = apply t (return f)
  end
end

(* Define [Monad] with a [Make] functor polymorphic over the portability
   of the functions using the corresponding [Applicative.Make] functor. *)
module Monad : sig
  module type Arg = sig
    type 'a t

    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end

  module type S = sig
    include Arg
    include Applicative.S with type 'a t := 'a t

    val join : 'a t t -> 'a t
  end

  module%template.portable Make (Arg : Arg) : S with type 'a t := 'a Arg.t
  [@@alert legacy "blah"]
  (* Test that additional attributes are preserved. *)
end = struct
  module type Arg = sig
    type 'a t

    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end

  module type S = sig
    include Arg
    include Applicative.S with type 'a t := 'a t

    val join : 'a t t -> 'a t
  end

  module%template.portable [@modality m] Make (Arg : Arg) : S with type 'a t := 'a Arg.t =
  struct
    include Arg

    include Applicative.Make [@modality m] (struct
        include Arg

        let apply t f = bind t ~f:(fun a -> bind f ~f:(fun f -> return (f a)))
      end)

    let join t = bind t ~f:(fun x -> x)
  end
end

(* Used below to force [return] and [bind] to be portable/nonportable. *)
  include%template (
  struct
    let maybe_nonportable () = () [@@modality m]
  end :
  sig
    val maybe_nonportable : unit -> unit [@@modality m]
  end) [@@modality m = (portable, nonportable)]

module%template.portable [@modality m] Option : sig
  type 'a t = 'a option =
    | None
    | Some of 'a

  include Monad.S with type 'a t := 'a t
end = struct
  module T = struct
    type 'a t = 'a option =
      | None
      | Some of 'a

    let return a =
      (maybe_nonportable [@modality m]) ();
      Some a
    ;;

    let bind t ~f =
      (maybe_nonportable [@modality m]) ();
      match t with
      | None -> None
      | Some a -> f a
    ;;
  end

  include T
  include Monad.Make [@modality m] [@alert "-legacy"] (T)
end

(* Show that the portable template satisfies [Monad.S] with portable functions. *)

module type%template [@modality m = (portable, nonportable)] Monad = sig
  include Monad.S
end

module%template [@modality m = (portable, nonportable)] _ : Monad [@modality m] =
  Option
  [@modality m]
