open! Base
open! Import
include Identifier_intf.Definitions

let make ~default =
  (module struct
    include String

    let default = default
  end : S
    with type t = string
     and type comparator_witness = String.comparator_witness)
;;

let kind = make ~default:"value"
let mode = make ~default:"global"

module Kind = (val kind)
module Mode = (val mode)
