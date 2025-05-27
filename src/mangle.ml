open! Stdppx
open! Import
open Language

(* Individual values are mangled based on their structures, and then concatenated together
   with double-underscores. *)
let rec mangle_value : type a. a Type.basic Value.t -> string =
  let group strings = strings |> String.concat ~sep:"_" |> Printf.sprintf "'%s'" in
  fun value ->
    match value with
    | Identifier ident -> ident.ident
    | Kind_product kinds -> List.map kinds ~f:mangle_value |> group
    | Kind_mod (kind, modifiers) ->
      mangle_value kind
      :: "mod"
      :: (List.map modifiers ~f:mangle_value |> List.sort_uniq ~cmp:String.compare)
      |> group
;;

let concat_with_underscores = String.concat ~sep:"__"

(* We check whether a value is a default value for its axis, as we don't include those
   in the name mangling scheme. *)
let is_default : type a. a Type.basic Value.t -> bool =
  fun value ->
  match Value.type_ value, value with
  | Basic Kind, Identifier { ident = "value" | "value_or_null"; _ } -> true
  | Basic Kind, _ -> false
  | ( Basic Mode
    , Identifier
        { ident = "global" | "nonportable" | "uncontended" | "aliased" | "many"; _ } ) ->
    true
  | Basic Mode, _ -> false
  | ( Basic Modality
    , Identifier
        { ident = "local" | "nonportable" | "uncontended" | "unique" | "once"; _ } ) ->
    true
  | Basic Modality, _ -> false
  | Basic Alloc, Identifier { ident = "heap"; _ } -> true
  | Basic Alloc, _ -> false
;;

module Suffix = struct
  type t = string list loc

  let create mono =
    let extract type_ =
      match Type.Map.find_opt (P type_) mono with
      | None -> []
      | Some manglers ->
        if List.for_all manglers ~f:(fun (Value.Basic.P value) -> is_default value)
        then []
        else List.map manglers ~f:(fun (Value.Basic.P value) -> mangle_value value)
    in
    { txt =
        extract Type.kind @ extract Type.mode @ extract Type.modality @ extract Type.alloc
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

let mangle (type a) (attr_ctx : a Attributes.Context.mono) (node : a) mangle_exprs ~env =
  if Type.Map.is_empty mangle_exprs
  then node
  else (
    let manglers =
      Type.Map.map
        (fun exprs ->
          List.map exprs ~f:(fun { txt = Expression.Basic.P expr; loc } ->
            Value.Basic.P (Env.eval env { txt = expr; loc })))
        mangle_exprs
    in
    let suffix = Suffix.create manglers in
    match attr_ctx with
    | Expression -> t#expression suffix node
    | Module_expr -> t#module_expr suffix node
    | Core_type -> t#core_type suffix node
    | Module_type -> t#module_type suffix node)
;;
