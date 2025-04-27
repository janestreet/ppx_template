open! Stdppx
open! Import

let concat_with_underscores = String.concat ~sep:"__"

module Suffix = struct
  type t = string list loc

  (* This code using [env] to map jkinds needs to live somewhere, and be called every time
     we go to mangle an identifier. Here seems like as reasonable a place as any. *)
  let create ~env ~kinds ~modes ~modalities =
    let resolve
      (type binding id node)
      ((module Binding) : (binding, id, node) Binding.t)
      bindings
      ~find
      =
      List.map bindings ~f:(fun binding ->
        binding
        |> Binding.resolve ~find_identifier:(find env)
        |> Binding.to_mangled_identifier)
    in
    let kinds = resolve Binding.kind kinds ~find:Env.find_kind in
    let modes = resolve Binding.mode modes ~find:Env.find_mode in
    let modalities = resolve Binding.modality modalities ~find:Env.find_modality in
    let txt (type id) ((module Id) : id Identifier.t) ids =
      if List.for_all ~f:(fun elt -> Id.Set.mem elt Id.defaults) ids
      then []
      else List.map ~f:Id.to_string ids
    in
    { txt =
        txt Identifier.kind kinds
        @ txt Identifier.mode modes
        @ txt Identifier.modality modalities
    ; loc = Location.none
    }
  ;;
end

let explicitly_drop = Attribute.explicitly_drop

let mangle_error { txt; loc } kind explicitly_drop_method node =
  explicitly_drop_method node;
  Location.error_extensionf
    ~loc
    "[%%template]: don't know how to mangle this %s (suffix: %s)"
    kind
    (concat_with_underscores txt)
;;

let t =
  object (self)
    inherit [Suffix.t] Ast_traverse.map_with_context as super
    method! string suffix name = concat_with_underscores (name :: suffix.txt)

    method! longident suffix =
      function
      | (Lident _ | Lapply _) as ident -> super#longident suffix ident
      | Ldot (path, name) -> Ldot (path, self#string suffix name)

    method! location _ = Fn.id

    method! expression_desc suffix =
      function
      | Pexp_ident _ as desc -> super#expression_desc suffix desc
      | expr_desc ->
        Pexp_extension
          (mangle_error suffix "expression" explicitly_drop#expression_desc expr_desc)

    method! expression suffix expr =
      { expr with
        pexp_desc =
          self#expression_desc { suffix with loc = expr.pexp_loc } expr.pexp_desc
      }

    method! module_expr_desc suffix =
      function
      | Pmod_ident _ as desc -> super#module_expr_desc suffix desc
      | mod_expr_desc ->
        Pmod_extension
          (mangle_error
             suffix
             "module expression"
             explicitly_drop#module_expr_desc
             mod_expr_desc)

    method! module_expr suffix mod_expr =
      { mod_expr with
        pmod_desc =
          self#module_expr_desc { suffix with loc = mod_expr.pmod_loc } mod_expr.pmod_desc
      }

    method! core_type_desc suffix =
      function
      | Ptyp_constr (name, params) ->
        Ptyp_constr (self#loc self#longident suffix name, params)
      | Ptyp_package (name, params) ->
        Ptyp_package (self#loc self#longident suffix name, params)
      | core_type_desc ->
        Ptyp_extension
          (mangle_error suffix "core type" explicitly_drop#core_type_desc core_type_desc)

    method! core_type suffix typ =
      { typ with
        ptyp_desc = self#core_type_desc { suffix with loc = typ.ptyp_loc } typ.ptyp_desc
      }

    method! module_type_desc suffix =
      function
      | Pmty_ident _ as desc -> super#module_type_desc suffix desc
      | mod_type_desc ->
        Pmty_extension
          (mangle_error
             suffix
             "module type"
             explicitly_drop#module_type_desc
             mod_type_desc)

    method! module_type suffix mod_typ =
      { mod_typ with
        pmty_desc =
          self#module_type_desc { suffix with loc = mod_typ.pmty_loc } mod_typ.pmty_desc
      }

    method! pattern_desc suffix pattern_desc =
      match Ppxlib_jane.Shim.Pattern_desc.of_parsetree pattern_desc with
      | Ppat_any | Ppat_var _ | Ppat_alias _ -> super#pattern_desc suffix pattern_desc
      | Ppat_constraint (pat, typ, modes) ->
        Ppat_constraint (self#pattern suffix pat, typ, modes)
        |> Ppxlib_jane.Shim.Pattern_desc.to_parsetree ~loc:pat.ppat_loc
      | _ ->
        (* If the user didn't request a polymorphic binding, or they only requested the
           [value] kinds, don't complain. *)
        if List.is_empty suffix.txt
        then pattern_desc
        else
          Ppat_extension
            (mangle_error suffix "pattern" explicitly_drop#pattern_desc pattern_desc)

    method! pattern suffix pat =
      { pat with
        ppat_desc = self#pattern_desc { suffix with loc = pat.ppat_loc } pat.ppat_desc
      }

    method! value_binding suffix binding =
      { binding with pvb_pat = self#pattern suffix binding.pvb_pat }

    method! value_description suffix desc =
      { desc with pval_name = self#loc self#string suffix desc.pval_name }

    method! module_binding suffix binding =
      { binding with
        pmb_name = self#loc (self#option self#string) suffix binding.pmb_name
      }

    method! module_declaration suffix decl =
      { decl with pmd_name = self#loc (self#option self#string) suffix decl.pmd_name }

    method! type_declaration suffix decl =
      { decl with ptype_name = self#loc self#string suffix decl.ptype_name }

    method! module_type_declaration suffix decl =
      { decl with pmtd_name = self#loc self#string suffix decl.pmtd_name }

    method! include_infos _ _ info = info
  end
;;
