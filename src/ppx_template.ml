open! Stdppx
open Ppx_template_expander
open Ppx_template_expander.Private
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
    Error
      (Syntax_error.createf
         ~loc
         "ppx_template: [%%template] extension is required to interpret attribute due to \
          %s flag"
         require_template_extension_flag)
  else Ok ()
;;

let mono_attrs =
  let map_context : type a. a Attribute_handler.Context.mono -> a Extension.Context.t
    = function
    | Core_type -> Core_type
    | Expression -> Expression
    | Module_expr -> Module_expr
    | Module_type -> Module_type
  in
  List.map Attribute_handler.Mono.contexts ~f:(fun (T ctxt) ->
    let extension_context = map_context ctxt in
    let find_exn attr = Attribute_handler.Attribute_map.find_exn attr ctxt in
    let maybe_explicit attr attr_explicit attr_explicit_plus_unmangled =
      Explicitness.Each.create (function
        | Explicit -> attr_explicit
        | Explicit_plus_unmangled -> attr_explicit_plus_unmangled
        | Drop_axis_if_all_defaults -> attr)
    in
    Context_free.Rule.attr_multiple_replace
      "template"
      extension_context
      (let open Attribute_handler.Mono in
       [ (find_exn kind_attr).drop_axis_if_all_defaults
       ; (find_exn kind_set_attr).drop_axis_if_all_defaults
       ; (find_exn mode_attr).drop_axis_if_all_defaults
       ; (find_exn modality_attr).drop_axis_if_all_defaults
       ; (find_exn alloc_attr).drop_axis_if_all_defaults
       ; (find_exn kind_attr).explicit
       ; (find_exn kind_set_attr).explicit
       ; (find_exn mode_attr).explicit
       ; (find_exn modality_attr).explicit
       ; (find_exn alloc_attr).explicit
       ; (find_exn kind_attr).explicit_plus_unmangled
       ; (find_exn kind_set_attr).explicit_plus_unmangled
       ; (find_exn mode_attr).explicit_plus_unmangled
       ; (find_exn modality_attr).explicit_plus_unmangled
       ; (find_exn alloc_attr).explicit_plus_unmangled
       ])
      (fun ~ctxt:_
        item
        [ kind
        ; kind_set
        ; mode
        ; modality
        ; alloc
        ; kind_explicit
        ; kind_set_explicit
        ; mode_explicit
        ; modality_explicit
        ; alloc_explicit
        ; kind_explicit_plus_unmangled
        ; kind_set_explicit_plus_unmangled
        ; mode_explicit_plus_unmangled
        ; modality_explicit_plus_unmangled
        ; alloc_explicit_plus_unmangled
        ] ->
        let loc = Attribute_handler.Context.location ctxt item in
        match
          Result.bind (check_if_bare_attributes_allowed ~loc) ~f:(fun () ->
            ([ ( P (Singleton Kind)
               , maybe_explicit kind kind_explicit kind_explicit_plus_unmangled )
             ; ( P (Set Kind)
               , maybe_explicit
                   kind_set
                   kind_set_explicit
                   kind_set_explicit_plus_unmangled )
             ; ( P (Singleton Mode)
               , maybe_explicit mode mode_explicit mode_explicit_plus_unmangled )
             ; ( P (Singleton Modality)
               , maybe_explicit
                   modality
                   modality_explicit
                   modality_explicit_plus_unmangled )
             ; ( P (Singleton Alloc)
               , maybe_explicit alloc alloc_explicit alloc_explicit_plus_unmangled )
             ]
             : (Language.Typed.Axis.packed * _) list)
            |> List.Or_first_error.filter_map ~f:(fun (type_, exprs) ->
              Explicitness.Each.combine exprs
              |> function
              | Ok None -> Ok None
              | Ok (Some exprs) ->
                let open Result.Let_syntax in
                let+ exprs = Explicitness.With.ok exprs in
                Some (type_, exprs)
              | Error `multiple ->
                Attribute_handler.error_you_can_only_use_one_attribute_per_axis ~loc))
        with
        | Ok type_expr_alist ->
          type_expr_alist
          |> Language.Typed.Axis.Map.of_list
          |> Mangle.mangle ctxt item ~env:Env.initial
        | Error err ->
          Syntax_error_conversion.to_extension_node
            (Attribute_handler.Context.mono_to_any ctxt)
            item
            err))
;;

let with_attr =
  [ Context_free.Rule.attr_replace
      "template"
      Module_type
      Attribute_handler.with_attr
      (fun ~ctxt:_ mty with_ ->
         let open Result.Let_syntax in
         match
           with_
           >>| Monomorphize.t#signature Monomorphize.Context.top
           >>= With_constraint.convert mty
         with
         | Ok mty -> mty
         | Error err -> Syntax_error_conversion.to_extension_node Module_type mty err)
  ]
;;

let () =
  Driver.register_transformation
    "template"
    ~extensions:[ structure; signature; expression; module_expr; module_type; core_type ]
    ~rules:(with_attr @ mono_attrs)
;;

let declare_portable_stateless_extensions context pattern f =
  List.map Portable_stateless.all ~f:(fun portable_stateless ->
    Extension.declare
      (Printf.sprintf "@template.%s" (Portable_stateless.to_string portable_stateless))
      context
      pattern
      (fun ~loc ~path:(_ : string) mod_ -> f portable_stateless ~loc ~mod_))
;;

let module_bindings =
  declare_portable_stateless_extensions
    Structure_item
    Ast_pattern.(pstr (pstr_module __ ^:: nil))
    (fun portable_stateless ~loc ~mod_ ->
      [%stri
        [%%template
          [%%i
            Portable_stateless.module_binding
              portable_stateless
              ~loc:{ loc with loc_ghost = true }
              ~mod_]]])
;;

let module_declarations =
  declare_portable_stateless_extensions
    Signature_item
    Ast_pattern.(psig (signature (psig_module __ ^:: nil)))
    (fun portable_stateless ~loc ~mod_ ->
      [%sigi:
        [%%template:
          [%%i
            Portable_stateless.module_declaration
              portable_stateless
              ~loc:{ loc with loc_ghost = true }
              ~mod_]]])
;;

let () =
  Driver.register_transformation
    "@template.portable"
    ~extensions:(module_bindings @ module_declarations)
;;

let registered = ()
