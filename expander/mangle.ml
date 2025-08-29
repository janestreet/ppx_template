open! Stdppx
open! Import
open Language.Typed

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
    | Tuple _ -> .
;;

let concat_with_underscores = String.concat ~sep:"__"
let map_fst (x, y) ~f = f x, y

module Suffix = struct
  type t = string list loc

  let is_empty t = List.is_empty t.txt

  let create mono =
    let extract axis =
      match (Axis.Map.find_opt (P axis) mono : _ Maybe_explicit.t option) with
      | None -> []
      | Some (Explicit, manglers) ->
        List.map manglers ~f:(fun (Value.Basic.P value) -> mangle_value value)
      | Some (Drop_axis_if_all_defaults, manglers) ->
        (* A map from axis to manglers. This is general because non-mode manglers should
           end up with a single axis. Non-mode axes can be specified in the future if
           necessary. *)
        let is_axis_all_default =
          List.fold_left
            manglers
            ~init:Axis.Sub_axis.Map.empty
            ~f:(fun map (Value.Basic.P value) ->
              Axis.Sub_axis.Map.add_to_list
                (P (Axis.Sub_axis.of_value value))
                (Value.is_default value)
                map)
          |> Axis.Sub_axis.Map.map (List.for_all ~f:Fn.id)
        in
        let manglers =
          List.filter manglers ~f:(fun (Value.Basic.P value) ->
            let axis = Axis.Sub_axis.of_value value in
            let is_all_default =
              (* We check whether an axis is all defaults, as we don't include those in the
                 name mangling scheme.

                 By construction, this is guaranteed to exist. We use the same axes in
                 manglers to create and access the map.
              *)
              Axis.Sub_axis.Map.find (P axis) is_axis_all_default
            in
            not is_all_default)
        in
        List.map manglers ~f:(fun (Value.Basic.P value) -> mangle_value value)
    in
    { txt = extract Kind @ extract Mode @ extract Modality @ extract Alloc
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

module Result = struct
  type t =
    | Did_not_mangle
    | Mangled

  let mangled_either a b =
    match a, b with
    | Did_not_mangle, Did_not_mangle -> Did_not_mangle
    | Did_not_mangle, Mangled | Mangled, Did_not_mangle | Mangled, Mangled -> Mangled
  ;;

  let mangled_any l = List.fold_left ~init:Did_not_mangle ~f:mangled_either l

  let did_mangle = function
    | Did_not_mangle -> false
    | Mangled -> true
  ;;
end

let t =
  object (self)
    inherit [Suffix.t, Result.t] Ast_traverse.lift_map_with_context as super
    method other _ _ = Did_not_mangle
    method unit _ () = (), Did_not_mangle
    method bool _ b = b, Did_not_mangle
    method char _ c = c, Did_not_mangle
    method int _ i = i, Did_not_mangle
    method int32 _ i = i, Did_not_mangle
    method int64 _ i = i, Did_not_mangle
    method nativeint _ i = i, Did_not_mangle
    method float _ f = f, Did_not_mangle

    method array f suffix a =
      let res, a =
        Array.fold_left_map a ~init:Did_not_mangle ~f:(fun acc x ->
          let x, res = f suffix x in
          Result.mangled_either acc res, x)
      in
      a, res

    method record _ r = r |> List.map ~f:snd |> Result.mangled_any
    method constr _ _ c = Result.mangled_any c
    method tuple _ t = Result.mangled_any t

    method string suffix name =
      if Suffix.is_empty suffix
      then name, Did_not_mangle
      else concat_with_underscores (name :: suffix.txt), Mangled

    method! longident suffix =
      function
      | (Lident _ | Lapply _) as ident -> super#longident suffix ident
      | Ldot (path, name) ->
        self#string suffix name |> map_fst ~f:(fun name -> Ldot (path, name))

    method! location _ loc = loc, Did_not_mangle

    method! extension suffix (ext, payload) =
      self#loc self#string suffix ext |> map_fst ~f:(fun ext -> ext, payload)

    method! expression_desc suffix =
      function
      | (Pexp_ident _ | Pexp_extension _) as desc -> super#expression_desc suffix desc
      | expr_desc ->
        ( Pexp_extension
            (mangle_error suffix "expression" explicitly_drop#expression_desc expr_desc)
        , Did_not_mangle )

    method! expression suffix expr =
      self#expression_desc { suffix with loc = expr.pexp_loc } expr.pexp_desc
      |> map_fst ~f:(fun pexp_desc -> { expr with pexp_desc })

    method! module_expr_desc suffix =
      function
      | Pmod_ident _ as desc -> super#module_expr_desc suffix desc
      | mod_expr_desc ->
        ( Pmod_extension
            (mangle_error
               suffix
               "module expression"
               explicitly_drop#module_expr_desc
               mod_expr_desc)
        , Did_not_mangle )

    method! module_expr suffix mod_expr =
      self#module_expr_desc { suffix with loc = mod_expr.pmod_loc } mod_expr.pmod_desc
      |> map_fst ~f:(fun pmod_desc -> { mod_expr with pmod_desc })

    method! core_type_desc suffix =
      function
      | Ptyp_constr (name, params) ->
        self#loc self#longident suffix name
        |> map_fst ~f:(fun name -> Ptyp_constr (name, params))
      | Ptyp_package (name, params) ->
        self#loc self#longident suffix name
        |> map_fst ~f:(fun name -> Ptyp_package (name, params))
      | Ptyp_extension _ as desc -> super#core_type_desc suffix desc
      | core_type_desc ->
        ( Ptyp_extension
            (mangle_error
               suffix
               "core type"
               explicitly_drop#core_type_desc
               core_type_desc)
        , Did_not_mangle )

    method! core_type suffix typ =
      self#core_type_desc { suffix with loc = typ.ptyp_loc } typ.ptyp_desc
      |> map_fst ~f:(fun ptyp_desc -> { typ with ptyp_desc })

    method! module_type_desc suffix =
      function
      | Pmty_ident _ as desc -> super#module_type_desc suffix desc
      | mod_type_desc ->
        ( Pmty_extension
            (mangle_error
               suffix
               "module type"
               explicitly_drop#module_type_desc
               mod_type_desc)
        , Did_not_mangle )

    method! module_type suffix mod_typ =
      self#module_type_desc { suffix with loc = mod_typ.pmty_loc } mod_typ.pmty_desc
      |> map_fst ~f:(fun pmty_desc -> { mod_typ with pmty_desc })

    method! pattern_desc suffix pattern_desc =
      match Ppxlib_jane.Shim.Pattern_desc.of_parsetree pattern_desc with
      | Ppat_any | Ppat_var _ | Ppat_alias _ -> super#pattern_desc suffix pattern_desc
      | Ppat_constraint (pat, typ, modes) ->
        self#pattern suffix pat
        |> map_fst ~f:(fun pat ->
          Ppxlib_jane.Shim.Pattern_desc.to_parsetree
            ~loc:pat.ppat_loc
            (Ppat_constraint (pat, typ, modes)))
      | _ ->
        (* If the user didn't request a polymorphic binding, or they only requested the
           [value] kinds, don't complain. *)
        ( (if List.is_empty suffix.txt
           then pattern_desc
           else
             Ppat_extension
               (mangle_error suffix "pattern" explicitly_drop#pattern_desc pattern_desc))
        , Did_not_mangle )

    method! pattern suffix pat =
      self#pattern_desc { suffix with loc = pat.ppat_loc } pat.ppat_desc
      |> map_fst ~f:(fun ppat_desc -> { pat with ppat_desc })

    method! value_binding suffix binding =
      self#pattern suffix binding.pvb_pat
      |> map_fst ~f:(fun pvb_pat -> { binding with pvb_pat })

    method! value_description suffix desc =
      self#loc self#string suffix desc.pval_name
      |> map_fst ~f:(fun pval_name -> { desc with pval_name })

    method! module_binding suffix binding =
      self#loc (self#option self#string) suffix binding.pmb_name
      |> map_fst ~f:(fun pmb_name -> { binding with pmb_name })

    method! module_declaration suffix decl =
      self#loc (self#option self#string) suffix decl.pmd_name
      |> map_fst ~f:(fun pmd_name -> { decl with pmd_name })

    method! type_declaration suffix decl =
      self#loc self#string suffix decl.ptype_name
      |> map_fst ~f:(fun ptype_name -> { decl with ptype_name })

    method! module_type_declaration suffix decl =
      self#loc self#string suffix decl.pmtd_name
      |> map_fst ~f:(fun pmtd_name -> { decl with pmtd_name })

    method! include_infos _ suffix info =
      let mangled : Result.t =
        if Suffix.is_empty suffix then Did_not_mangle else Mangled
      in
      info, mangled
  end
;;

let mangle (type a) (attr_ctx : a Attributes.Context.mono) (node : a) mangle_exprs ~env =
  if Axis.Map.is_empty mangle_exprs
  then node
  else (
    let manglers =
      Axis.Map.map
        (Maybe_explicit.map
           ~f:
             (List.map ~f:(fun { txt = Expression.Basic.P expr; loc } ->
                Value.Basic.P (Env.eval env { txt = expr; loc }))))
        mangle_exprs
    in
    let suffix = Suffix.create manglers in
    let (node : a), (_ : Result.t) =
      match attr_ctx with
      | Expression -> t#expression suffix node
      | Module_expr -> t#module_expr suffix node
      | Core_type -> t#core_type suffix node
      | Module_type -> t#module_type suffix node
    in
    node)
;;
