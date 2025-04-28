open! Stdppx
open! Import

(** For use in other ppxes. *)
module Export : sig
  module Monomorphize = Monomorphize
end
