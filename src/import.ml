include struct
  open Ppxlib
  include Ast
  module Ast_builder = Ast_builder.Default

  module Ast_pattern = struct
    include Ast_pattern

    (* [fix f] computes the fixpoint of [f]. *)
    let fix f =
      let rec t ctx loc x k = (to_func (f (of_func t))) ctx loc x k in
      f (of_func t)
    ;;
  end

  module Attribute = Attribute
  module Driver = Driver
  module Extension = Extension
  module Loc = Loc
  module Location = Location
end

include struct
  open Ppxlib_jane
  module Ast_traverse = Ast_traverse

  type jkind_annotation = Shim.jkind_annotation
  type mode = Shim.Mode.t = Mode of string [@@unboxed]
end

include Composition_infix
