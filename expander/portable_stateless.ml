open! Stdppx
open! Import

type t =
  | Portable
  | Stateless

let all = [ Portable; Stateless ]

let to_string = function
  | Portable -> "portable"
  | Stateless -> "stateless"
;;

(* Convert [functor (X1 : S1) (X2 : S2) -> S3] to
   {[
     functor
       (X1 : sig @@ portable include S1 end)
       (X2 : sig @@ portable include S2 end)
       -> sig @@ portable include S3 end
   ]}
*)
let mk_pmb_type pmb_type ~loc ~mvar =
  let rec loop pmb_type =
    match Ppxlib_jane.Shim.Module_type_desc.of_parsetree pmb_type.pmty_desc with
    | Pmty_functor (param, ret_type, modes) ->
      let param =
        match Ppxlib_jane.Shim.Functor_parameter.of_parsetree param with
        | Unit -> Unit
        | Named (name, pmb_type, modes) ->
          Ppxlib_jane.Shim.Functor_parameter.to_parsetree
            (Named (name, loop pmb_type, modes))
      in
      { pmb_type with
        pmty_desc =
          Ppxlib_jane.Shim.Module_type_desc.to_parsetree
            ~loc
            (Pmty_functor (param, loop ret_type, modes))
      ; pmty_loc = loc
      }
    | Pmty_signature signature ->
      let signature = Ppxlib_jane.Shim.Signature.of_parsetree signature in
      { pmb_type with
        pmty_desc =
          Ppxlib_jane.Shim.Module_type_desc.to_parsetree
            ~loc
            (Pmty_signature
               (Ppxlib_jane.Shim.Signature.to_parsetree
                  { signature with
                    psg_modalities =
                      Loc.map mvar ~f:(fun x -> Modality x) :: signature.psg_modalities
                  ; psg_loc = loc
                  }))
      ; pmty_loc = loc
      }
    | _ ->
      Ast_builder.pmty_signature
        ~loc
        (Ast_builder.signature
           ~loc
           ~modalities:[ Loc.map mvar ~f:(fun x -> Modality x) ]
           [ Ast_builder.psig_include
               ~loc
               ~modalities:[]
               (Ast_builder.include_infos ~loc ~kind:Structure pmb_type)
           ])
  in
  loop pmb_type
;;

(* Convert [functor (X1 : S1) (X2 : S2) : S3 -> ...] to
   {[
     functor
       (X1 : sig @@ portable include S1 end)
       (X2 : sig @@ portable include S2 end)
       : sig @@ portable include S3 end
       ->
         ...
   ]}
*)
let mk_pmb_expr pmb_expr ~loc ~mvar =
  let rec loop pmb_expr =
    match Ppxlib_jane.Shim.Module_expr_desc.of_parsetree pmb_expr.pmod_desc with
    | Pmod_constraint (pmb_expr, pmb_type, modes) ->
      { pmb_expr with
        pmod_desc =
          Ppxlib_jane.Shim.Module_expr_desc.to_parsetree
            ~loc
            (Pmod_constraint
               (loop pmb_expr, Option.map pmb_type ~f:(mk_pmb_type ~loc ~mvar), modes))
      ; pmod_loc = loc
      }
    | Pmod_functor (param, pmb_expr) ->
      let param =
        match Ppxlib_jane.Shim.Functor_parameter.of_parsetree param with
        | Unit -> Unit
        | Named (name, pmb_type, modes) ->
          Ppxlib_jane.Shim.Functor_parameter.to_parsetree
            (Named (name, mk_pmb_type pmb_type ~loc ~mvar, modes))
      in
      { pmb_expr with pmod_desc = Pmod_functor (param, loop pmb_expr); pmod_loc = loc }
    | _ -> pmb_expr
  in
  loop pmb_expr
;;

let mvals ~loc = function
  | Portable -> [%expr portable, nonportable]
  | Stateless -> [%expr stateless, reading, stateful]
;;

(* [@@template.modality `mvar` = `mvals ~loc t`] *)
let modality_attribute t ~loc ~mvar =
  Ast_builder.attribute
    ~loc
    ~name:(Loc.make ~loc "template.modality")
    ~payload:
      (PStr [%str [%e Ast_builder.evar ~loc:mvar.loc mvar.txt] = [%e mvals ~loc t]])
;;

let attr_handler = function
  | Portable -> Attribute_handler.functor_portable
  | Stateless -> Attribute_handler.functor_stateless
;;

let mk_error
  : type a.
    ctx:(a, [ `module_binding | `module_declaration ]) Attribute_handler.Context.t
    -> error:Syntax_error.t
    -> loc:location
    -> mod_:a
    -> a
  =
  fun ~ctx ~error ~loc ~mod_ ->
  match ctx with
  | Module_binding ->
    { mod_ with
      pmb_expr = Ast_builder.pmod_extension ~loc (Syntax_error.to_extension error)
    }
  | Module_declaration ->
    { mod_ with
      pmd_type = Ast_builder.pmty_extension ~loc (Syntax_error.to_extension error)
    }
;;

let gensym ~loc = { loc; txt = Ppxlib.gen_symbol ~prefix:"modality" () }

let handle_attribute t ctx ~loc ~mod_ =
  match Attribute_handler.consume (attr_handler t) ctx mod_ with
  | Ok (mod_, Some mvar) -> mod_, mvar
  | Ok (mod_, None) -> mod_, gensym ~loc
  | Error error -> mk_error ~ctx ~error ~loc ~mod_, gensym ~loc
;;

let module_binding t ~loc ~mod_ =
  let loc = { loc with loc_ghost = true } in
  let mod_, mvar = handle_attribute t Module_binding ~loc ~mod_ in
  Ast_builder.pstr_module
    ~loc
    { mod_ with
      pmb_expr = mk_pmb_expr mod_.pmb_expr ~loc ~mvar
    ; pmb_attributes = modality_attribute t ~loc ~mvar :: mod_.pmb_attributes
    }
;;

let module_declaration t ~loc ~mod_ =
  let loc = { loc with loc_ghost = true } in
  let mod_, mvar = handle_attribute t Module_declaration ~loc ~mod_ in
  Ast_builder.psig_module
    ~loc
    { mod_ with
      pmd_type = mk_pmb_type mod_.pmd_type ~loc ~mvar
    ; pmd_attributes = modality_attribute t ~loc ~mvar :: mod_.pmd_attributes
    }
;;
