open! Stdppx
open! Import
open Language.Typed
module Type = Language.Type

(* Individual values are mangled based on their structures, and then concatenated together
   with double-underscores. *)
let rec mangle_value : type a. a Type.non_tuple Value.t -> string =
  let group strings = strings |> String.concat ~sep:"_" |> Printf.sprintf "'%s'" in
  fun value ->
    match value with
    | Identifier ident -> ident.ident
    | Kind_product kinds ->
      kinds |> Nonempty_list.to_list |> List.map ~f:mangle_value |> group
    | Kind_mod (kind, modifiers) ->
      mangle_value kind
      :: "mod"
      :: (modifiers
          |> Nonempty_list.to_list
          |> List.map ~f:mangle_value
          |> List.sort_uniq ~cmp:String.compare)
      |> group
;;

let concat_with_underscores = String.concat ~sep:"__"

module Suffix = struct
  type t = string list loc

  let is_empty t = List.is_empty t.txt

  let create mono =
    let open Explicitness.With.Export in
    let extract axis =
      match Axis.Map.find_opt (P axis) mono with
      | None -> []
      | Some { explicitness; what = manglers } ->
        List.fold_right
          manglers
          ~init:Axis.Sub_axis.Map.empty
          ~f:(fun (Value.Basic.P value as mangler) map ->
            Axis.Sub_axis.Map.add_to_list (P (Axis.Sub_axis.of_value value)) mangler map)
        |> Axis.Sub_axis.Map.to_list
        |> List.concat_map ~f:(fun (_, manglers) ->
          (* We check whether an axis is all defaults, as we don't include those in the
             name mangling scheme. *)
          match (explicitness : Explicitness.t) with
          | Drop_axis_if_all_defaults
            when List.for_all manglers ~f:(fun (Value.Basic.P value) ->
                   match Value.defaultness value with
                   | Actual_default | Default_for_standard_mangling -> true
                   | Not_a_default -> false) -> []
          | Drop_axis_if_all_defaults | Explicit | Explicit_plus_unmangled ->
            List.map manglers ~f:(fun (P value) ->
              let mangler = mangle_value value in
              (* sets are surrounded with additional [''] to disambiguate from the non-set
                 versions *)
              match axis with
              | Set _ -> Printf.sprintf "''%s''" mangler
              | Singleton _ -> mangler))
    in
    { txt =
        extract (Set Kind)
        @ extract (Singleton Kind)
        @ extract (Singleton Mode)
        @ extract (Singleton Modality)
        @ extract (Singleton Alloc)
        @ extract (Singleton Synchro)
    ; loc = Location.none
    }
  ;;
end

let suffix_for_manual_mangling ?modes ?kinds () =
  let kind_explicitness, (kinds : jkind_annotation list) =
    Option.value kinds ~default:(Explicitness.Drop_axis_if_all_defaults, [])
  in
  let mode_explicitness, (modes : modes) =
    Option.value modes ~default:(Explicitness.Drop_axis_if_all_defaults, [])
  in
  let open Result.Let_syntax in
  let* kinds =
    List.map kinds ~f:(fun ({ pjka_loc; _ } as jkind) ->
      let* jkind =
        Expression.of_parsetree_jkind jkind
        |> Result.map_error ~f:(fun { loc; txt } ->
          Syntax_error.createf ~loc "%s expected" txt)
      in
      let+ jkind = Env.eval_singleton Env.initial { txt = jkind; loc = pjka_loc } in
      (P jkind : Value.Basic.packed))
    |> Result.syntax_errors_of_list
  in
  let+ modes =
    List.map modes ~f:(fun { txt = Mode mode; loc } ->
      let+ mode =
        Env.eval_singleton
          Env.initial
          { txt = Identifier { type_ = Non_tuple Mode; ident = mode }; loc }
      in
      (P mode : Value.Basic.packed))
    |> Result.syntax_errors_of_list
  in
  let suffix =
    Suffix.create
      (Axis.Map.of_list
         [ ( P (Singleton Kind)
           , { Explicitness.With.explicitness = kind_explicitness; what = kinds } )
         ; ( P (Singleton Mode)
           , { Explicitness.With.explicitness = mode_explicitness; what = modes } )
         ])
  in
  String.concat ~sep:"__" ("" :: suffix.txt)
;;

let explicitly_drop = Attribute.explicitly_drop

let mangle_error { txt; loc } kind explicitly_drop_method node =
  explicitly_drop_method node;
  Location.error_extensionf
    ~loc
    "[%%template]: don't know how to mangle this %s (suffix: %s)"
    kind
    (concat_with_underscores txt)
;;

module Outcome = struct
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
    inherit [Suffix.t, Outcome.t] Ast_traverse.lift_map_with_context as super
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
          Outcome.mangled_either acc res, x)
      in
      a, res

    method record _ r = r |> List.map ~f:snd |> Outcome.mangled_any
    method constr _ _ c = Outcome.mangled_any c
    method tuple _ t = Outcome.mangled_any t

    method string suffix name =
      if Suffix.is_empty suffix
      then name, Did_not_mangle
      else (
        let name =
          if String.ends_with ~suffix:"#" name
          then concat_with_underscores (String.drop_suffix name 1 :: suffix.txt) ^ "#"
          else concat_with_underscores (name :: suffix.txt)
        in
        name, Mangled)

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
      let mangled : Outcome.t =
        if Suffix.is_empty suffix then Did_not_mangle else Mangled
      in
      info, mangled
  end
;;

let mangle
  (type a)
  (attr_ctx : a Attribute_handler.Context.mono)
  (node : a)
  mangle_exprs
  ~env
  =
  if Axis.Map.is_empty mangle_exprs
  then node
  else (
    let results =
      Axis.Map.map
        (fun me ->
          Explicitness.With.map me ~f:(fun exprs ->
            List.Or_first_error.map
              exprs
              ~f:(fun { txt = Expression.Basic.P expr; loc } ->
                Result.map
                  (Env.eval_singleton env { txt = expr; loc })
                  ~f:(fun value -> Value.Basic.P value)))
          |> Explicitness.With.ok)
        mangle_exprs
    in
    let manglers =
      Axis.Map.filter_map
        (fun _ -> function
          | Ok x -> Some x
          | Error _ -> None)
        results
    in
    let errors =
      Axis.Map.filter_map
        (fun _ -> function
          | Ok _ -> None
          | Error e -> Some e)
        results
    in
    match Axis.Map.to_list errors with
    | [] ->
      let suffix = Suffix.create manglers in
      let (node : a), (_ : Outcome.t) =
        match attr_ctx with
        | Expression -> t#expression suffix node
        | Module_expr -> t#module_expr suffix node
        | Core_type -> t#core_type suffix node
        | Module_type -> t#module_type suffix node
      in
      node
    | [ (_, err) ] ->
      Syntax_error_conversion.to_extension_node
        (Attribute_handler.Context.mono_to_any attr_ctx)
        node
        err
    | (_, err) :: (_ :: _ as alist) ->
      Syntax_error_conversion.to_extension_node
        (Attribute_handler.Context.mono_to_any attr_ctx)
        node
        (Syntax_error.combine (err :: List.map alist ~f:snd)))
;;
