open! Base
open! Import

let structure =
  Extension.declare
    "template"
    Structure_item
    Ast_pattern.(pstr __)
    (fun ~loc ~path:(_ : string) stris ->
      match Monomorphize.t#structure Monomorphize.Context.top stris with
      | [ stri ] -> stri
      | stris ->
        let mod_ = Ast_builder.pmod_structure ~loc stris in
        let loc = { loc with loc_ghost = true } in
        [%stri include [%m mod_]])
;;

let signature =
  Extension.declare
    "template"
    Signature_item
    Ast_pattern.(psig __)
    (fun ~loc ~path:(_ : string) sigis ->
      match Monomorphize.t#signature Monomorphize.Context.top sigis with
      | [ sigi ] -> sigi
      | sigis ->
        let mod_ = Ast_builder.pmty_signature ~loc sigis in
        let loc = { loc with loc_ghost = true } in
        [%sigi: include [%m mod_]])
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
      Ast_builder.pmty_signature
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

let () =
  Driver.register_transformation
    "template"
    ~extensions:[ structure; signature; expression; module_expr; module_type; core_type ]
;;
