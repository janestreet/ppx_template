open! Base
open! Import

module Definitions = struct
  module type S = sig
    type t

    val compare : t -> t -> int
    val equal : t -> t -> bool
    val sexp_of_t : t -> Sexp.t

    include Comparator.S with type t := t
    include Stringable.S with type t := t

    val default : t
  end

  type ('a, 'cmp) t = (module S with type t = 'a and type comparator_witness = 'cmp)

  module M (Id : S) = struct
    type nonrec t = (Id.t, Id.comparator_witness) t
  end
end

module type Identifier = sig
  include module type of struct
    include Definitions
  end

  module Kind : S
  module Mode : S

  val kind : M(Kind).t
  val mode : M(Mode).t
end
