open! Stdppx
open! Import

let structure =
  Extension.declare_inline
    "template"
    Structure_item
    Ast_pattern.(pstr __)
    (fun ~loc:(_ : location) ~path:(_ : string) stris ->
      Monomorphize.t#structure Monomorphize.Context.top stris)
;;

let signature =
  Extension.declare_inline
    "template"
    Signature_item
    Ast_pattern.(psig __)
    (fun ~loc:(_ : location) ~path:(_ : string) sigis ->
      (Ppxlib_jane.Shim.Signature.of_parsetree
         (Monomorphize.t#signature Monomorphize.Context.top sigis))
        .psg_items)
;;

let expression =
  Extension.declare
    "template"
    Expression
    Ast_pattern.(pstr (pstr_eval __ drop ^:: nil))
    (fun ~loc:(_ : location) ~path:(_ : string) expr ->
      Monomorphize.t#expression Monomorphize.Context.top expr)
;;

let module_expr =
  Extension.declare
    "template"
    Module_expr
    Ast_pattern.(pstr __)
    (fun ~loc ~path:(_ : string) stris ->
      Ast_builder.pmod_structure
        ~loc:{ loc with loc_ghost = true }
        (Monomorphize.t#structure Monomorphize.Context.top stris))
;;

let module_type =
  Extension.declare
    "template"
    Module_type
    Ast_pattern.(psig __)
    (fun ~loc ~path:(_ : string) sigis ->
      Ppxlib_jane.Ast_builder.Default.pmty_signature
        ~loc:{ loc with loc_ghost = true }
        (Monomorphize.t#signature Monomorphize.Context.top sigis))
;;

let core_type =
  Extension.declare
    "template"
    Core_type
    Ast_pattern.(ptyp __)
    (fun ~loc:(_ : location) ~path:(_ : string) typ ->
      Monomorphize.t#core_type Monomorphize.Context.top typ)
;;

let require_template_extension = ref false
let require_template_extension_flag = "-require-template-extension"

let () =
  Driver.add_arg
    require_template_extension_flag
    (Set require_template_extension)
    ~doc:"disallow bare ppx_template attributes"
;;

let check_if_bare_attributes_allowed ~loc =
  if !require_template_extension
  then
    Location.raise_errorf
      ~loc
      "ppx_template: [%%template] extension is required to interpret attribute due to %s \
       flag"
      require_template_extension_flag
;;

let mono_attrs = []

let () =
  Driver.register_transformation
    "template"
    ~extensions:[ structure; signature; expression; module_expr; module_type; core_type ]
    ~rules:mono_attrs
;;

let module_binding =
  Extension.declare
    "@template.portable"
    Structure_item
    Ast_pattern.(pstr (pstr_module __ ^:: nil))
    (fun ~loc ~path:(_ : string) mod_ ->
      [%stri
        [%%template [%%i Portable.module_binding ~loc:{ loc with loc_ghost = true } mod_]]])
;;

let module_declaration =
  Extension.declare
    "@template.portable"
    Signature_item
    Ast_pattern.(psig (signature (psig_module __ ^:: nil)))
    (fun ~loc ~path:(_ : string) mod_ ->
      [%sigi:
        [%%template:
          [%%i Portable.module_declaration ~loc:{ loc with loc_ghost = true } mod_]]])
;;

let () =
  Driver.register_transformation
    "@template.portable"
    ~extensions:[ module_binding; module_declaration ]
;;

module Export = struct
  module Monomorphize = Monomorphize
end
