include struct
  open Ppxlib
  include Ast

  module Ast_pattern = struct
    include Ast_pattern

    (* [fix f] computes the fixpoint of [f]. *)
    let fix f =
      let rec t ctx loc x k = (to_func (f (of_func t))) ctx loc x k in
      f (of_func t)
    ;;
  end

  module Attribute = Attribute
  module Context_free = Context_free
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
  type modality = Shim.Modality.t = Modality of string [@@unboxed]
end

module Ast_builder = struct
  include Ppxlib.Ast_builder.Default
  include Ppxlib_jane.Ast_builder.Default
end

include struct
  open Sexplib0
  module Sexp = Sexp
  include Sexp_conv
end
