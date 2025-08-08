open! Stdppx
open! Import

type t = Sexp.t

let to_extension ~loc err explicitly_drop_method node =
  explicitly_drop_method node;
  Ppxlib.Location.error_extensionf
    ~loc
    "%s"
    (Sexp.to_string_hum (Sexp.message "[%template]" [ "", err ]))
;;

let to_extension_node : type a. a Attributes.Context.any -> a -> t -> a =
  fun ctx node t ->
  let extension, loc, explicitly_drop =
    match ctx with
    | Expression ->
      ( (Ast_builder.pexp_extension : loc:_ -> Ppxlib.extension -> a)
      , node.pexp_loc
      , (Attribute.explicitly_drop#expression : a -> unit) )
    | Module_expr ->
      Ast_builder.pmod_extension, node.pmod_loc, Attribute.explicitly_drop#module_expr
    | Core_type ->
      Ast_builder.ptyp_extension, node.ptyp_loc, Attribute.explicitly_drop#core_type
    | Module_type ->
      Ast_builder.pmty_extension, node.pmty_loc, Attribute.explicitly_drop#module_type
    | Value_binding ->
      ( (fun ~loc ext -> { node with pvb_expr = Ast_builder.pexp_extension ~loc ext })
      , node.pvb_loc
      , Attribute.explicitly_drop#value_binding )
    | Value_description ->
      ( (fun ~loc ext -> { node with pval_type = Ast_builder.ptyp_extension ~loc ext })
      , node.pval_loc
      , Attribute.explicitly_drop#value_description )
    | Module_binding ->
      ( (fun ~loc ext -> { node with pmb_expr = Ast_builder.pmod_extension ~loc ext })
      , node.pmb_loc
      , Attribute.explicitly_drop#module_binding )
    | Module_declaration ->
      ( (fun ~loc ext -> { node with pmd_type = Ast_builder.pmty_extension ~loc ext })
      , node.pmd_loc
      , Attribute.explicitly_drop#module_declaration )
    | Type_declaration ->
      ( (fun ~loc ext ->
          { node with ptype_manifest = Some (Ast_builder.ptyp_extension ~loc ext) })
      , node.ptype_loc
      , Attribute.explicitly_drop#type_declaration )
    | Module_type_declaration ->
      ( (fun ~loc ext ->
          { node with pmtd_type = Some (Ast_builder.pmty_extension ~loc ext) })
      , node.pmtd_loc
      , Attribute.explicitly_drop#module_type_declaration )
    | Include_infos ->
      ( (fun ~loc ext ->
          match node.pincl_mod with
          | Left _ -> { node with pincl_mod = Left (Ast_builder.pmod_extension ~loc ext) }
          | Right _ ->
            { node with pincl_mod = Right (Ast_builder.pmty_extension ~loc ext) })
      , node.pincl_loc
      , Attribute.explicitly_drop#include_infos (function
          | (Left mod_ : _ Either.t) -> Attribute.explicitly_drop#module_expr mod_
          | Right mty -> Attribute.explicitly_drop#module_type mty) )
  in
  let loc = { loc with loc_ghost = true } in
  extension ~loc (to_extension ~loc t explicitly_drop node)
;;

let to_extension_node_floating
  : type a. a Attributes.Floating.Context.poly -> loc:location -> t -> a
  =
  fun ctx ~loc t ->
  match ctx with
  | Structure_item ->
    Ast_builder.pstr_extension ~loc (to_extension ~loc t (fun _ -> ()) ()) []
  | Signature_item ->
    Ast_builder.psig_extension ~loc (to_extension ~loc t (fun _ -> ()) ()) []
;;
