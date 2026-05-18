open! Stdppx
open! Import
open Language.Typed
module Type = Language.Type
open Result.Let_syntax
include Monomorphize_intf.Definitions

module Context = struct
  module Defaults = struct
    type t = Value.Basic.packed list Explicitness.With.t Axis.Map.t

    let empty = Axis.Map.empty
  end

  type t =
    { ghostify : bool
    ; env : Env.t
    ; defaults : Defaults.t
    }

  let top = { ghostify = false; env = Env.initial; defaults = Defaults.empty }
end

module Maybe_hide_in_docs = struct
  type 'a t =
    { node : 'a
    ; hide_in_docs : bool
    }

  let node t = t.node
  let hide_in_docs t = t.hide_in_docs
end

let extract_loc (type a) (ctx : a Attribute_handler.Context.poly) (node : a) =
  match ctx with
  | Value_binding -> node.pvb_loc
  | Value_description -> node.pval_loc
  | Module_binding -> node.pmb_loc
  | Module_declaration -> node.pmd_loc
  | Type_declaration -> node.ptype_loc
  | Module_type_declaration -> node.pmtd_loc
  | Include_infos -> node.pincl_loc
;;

let extract_attrs (type a) (ctx : a Attribute_handler.Context.poly) (node : a) =
  match ctx with
  | Value_binding -> node.pvb_attributes
  | Value_description -> node.pval_attributes
  | Module_binding -> node.pmb_attributes
  | Module_declaration -> node.pmd_attributes
  | Type_declaration -> node.ptype_attributes
  | Module_type_declaration -> node.pmtd_attributes
  | Include_infos -> node.pincl_attributes
;;

type error =
  { loc : Location.t
  ; err : Syntax_error.t
  ; attrs : attributes
  }

let extract_bindings (type a) (ctx : a Attribute_handler.Context.poly) node =
  match Attribute_handler.consume Attribute_handler.poly ctx node with
  | Ok (node, bindings) -> Ok (node, bindings)
  | Error err -> Error { loc = extract_loc ctx node; err; attrs = extract_attrs ctx node }
;;

let merge_explicitness_or_error ~loc ~context old_explicitness new_explicitness =
  if Explicitness.equal old_explicitness new_explicitness
  then Ok old_explicitness
  else
    Error
      (Syntax_error.createf
         ~loc
         "[%%template]: explicitness mismatch while %s (%s vs %s)"
         context
         (Explicitness.to_string old_explicitness)
         (Explicitness.to_string new_explicitness))
;;

let merge_defaults_or_error ~loc ~context old_defaults new_defaults =
  let open Explicitness.With.Export in
  List.fold_left
    (Axis.Map.to_list new_defaults)
    ~init:(Ok old_defaults)
    ~f:(fun merged (axis, { explicitness = new_explicitness; what = new_values }) ->
      let* merged = merged in
      match Axis.Map.find_opt axis merged with
      | None ->
        Ok
          (Axis.Map.add
             axis
             { explicitness = new_explicitness; what = new_values }
             merged)
      | Some { explicitness = old_explicitness; what = old_values } ->
        let* explicitness =
          match old_values, new_values with
          | _, [] ->
            Error
              (Syntax_error.createf ~loc "empty floating poly attributes are not allowed")
          | [], _ :: _ ->
            (* [old_values] will be empty in the initial context *)
            Ok new_explicitness
          | _ :: _, _ :: _ ->
            merge_explicitness_or_error ~loc ~context old_explicitness new_explicitness
        in
        Ok (Axis.Map.add axis { explicitness; what = old_values @ new_values } merged))
;;

type 'a instance =
  { node : 'a
  ; env : Env.t
  ; manglers : Value.Basic.packed list Explicitness.With.t Axis.Map.t
  }

(* For Explicit_plus_unmangled axes with "strict default" (excludes value_or_null) values,
   return bindings that don't mangle those defaults *)
let expand_explicit_plus_unmangled ({ node = _; env = _; manglers } as instance) =
  let open Explicitness.With.Export in
  let explicit_plus_unmangled_requested =
    (* we only consider generating extra untemplated bindings if *any* axes are
       [Explicit_plus_unmangled] *)
    Axis.Map.exists
      (fun _ { explicitness; what = _ } ->
        match explicitness with
        | Explicitness.Explicit_plus_unmangled -> true
        | _ -> false)
      manglers
  in
  let is_strict_default =
    (* we only generate the untemplated binding when *all* axes that are
       [Explicit_plus_unmangled] are strict defaults *)
    Axis.Map.for_all
      (fun _ { explicitness; what = values } ->
        match explicitness with
        | Explicitness.Explicit_plus_unmangled ->
          List.for_all values ~f:(fun (Value.Basic.P v) ->
            match Value.defaultness v with
            | Actual_default -> true
            | Default_for_standard_mangling | Not_a_default -> false)
        | _ -> true)
      manglers
  in
  match explicit_plus_unmangled_requested && is_strict_default with
  | false -> Nonempty_list.singleton instance
  | true ->
    let dropped_manglers =
      Axis.Map.map
        (fun { explicitness; what = values } ->
          match explicitness with
          | Explicitness.Explicit_plus_unmangled ->
            { explicitness = Drop_axis_if_all_defaults; what = values }
          | _ -> { explicitness; what = values })
        manglers
    in
    [ instance; { instance with manglers = dropped_manglers } ]
;;

let instantiate poly_attributes ~env:(original_env : Env.t) =
  Env.instantiate original_env poly_attributes
  >>| Nonempty_list.map ~f:(fun (env, manglers) -> { node = (); env; manglers })
;;

let consume_poly
  (type a)
  ~env
  ~(defaults : Context.Defaults.t)
  (attr_ctx : a Attribute_handler.Context.poly)
  (nodes : a Nonempty_list.t)
  : (a instance Nonempty_list.t, error) result
  =
  Nonempty_list.Or_first_error.concat_map nodes ~f:(fun node ->
    match extract_bindings attr_ctx node with
    | Error _ as err -> err
    | Ok (node, polys) ->
      (match instantiate polys ~env with
       | Error err ->
         Error
           { loc = extract_loc attr_ctx node; err; attrs = extract_attrs attr_ctx node }
       | Ok list ->
         let instances =
           Nonempty_list.map list ~f:(fun { node = (); env; manglers } ->
             (* Use current default manglers when axis is unspecified. *)
             let manglers =
               Axis.Map.merge
                 (fun _ defaults manglers ->
                   match defaults, manglers with
                   | None, None -> None
                   | _, Some _ -> manglers
                   | Some _, None -> defaults)
                 defaults
                 manglers
             in
             (* Associate node with the instance *)
             { node; env; manglers })
         in
         Ok instances))
;;

(* Ppxlib individually applies every [@@deriving] or [@@deriving_inline] attribute it
   encounters to all type declarations in the same group, so if we naively copy them to
   each synthetic declaration produced by ppx_template, we end up with many copies of the
   same code. To fix this, we deduplicate attributes from the same source location. *)
let deduplicate_deriving_attrs tds =
  let module Location_ignoring_ghost =
    Set.Make (struct
      type t = location

      let compare a b =
        match Location.compare_pos a.loc_start b.loc_start with
        | 0 -> Location.compare_pos a.loc_end b.loc_end
        | c -> c
      ;;
    end)
  in
  let seen = ref Location_ignoring_ghost.empty in
  List.map tds ~f:(fun td ->
    { td with
      ptype_attributes =
        List.filter td.ptype_attributes ~f:(function
          | { attr_name =
                { txt =
                    ( "ppxlib.deriving"
                    | "ppxlib.deriving_inline"
                    | "deriving"
                    | "deriving_inline" )
                ; loc
                }
            ; _
            } ->
            if Location_ignoring_ghost.mem loc !seen
            then false
            else (
              seen := Location_ignoring_ghost.add loc !seen;
              true)
          | _ -> true)
    })
;;

let is_local (mode : (Type.mode, Expression.singleton) Expression.t Loc.t) ~env =
  let loc = mode.loc in
  Result.bind (Env.eval_singleton env mode) ~f:(fun (Identifier ident) ->
    match ident.ident with
    | "local" -> Ok true
    | "global" -> Ok false
    | mode ->
      Error (Syntax_error.createf ~loc "Unknown or invalid mode identifier: %s" mode))
;;

let is_stack (alloc : (Type.alloc, Expression.singleton) Expression.t Loc.t) ~env =
  let loc = alloc.loc in
  Result.bind (Env.eval_singleton env alloc) ~f:(fun (Identifier ident) ->
    match ident.ident with
    | "stack" -> Ok true
    | "heap" -> Ok false
    | alloc -> Error (Syntax_error.createf ~loc "Unbound alloc identifier: %s" alloc))
;;

let should_wrap_with_exclave
  expr
  ~(exclave_because_stack : _ Attribute_handler.Exclave_if.t option)
  ~(exclave_because_local : _ Attribute_handler.Exclave_if.t option)
  ~env
  =
  let stack_result =
    match exclave_because_stack with
    | None -> None
    | Some { expr = alloc; reasons = _ } ->
      (match is_stack alloc ~env with
       | (Ok true | Error _) as result ->
         Some
           (Result.map_error
              result
              ~f:(Syntax_error_conversion.to_extension_node Expression expr))
       | Ok false -> None)
  in
  match stack_result with
  | Some result -> result
  | None ->
    (match exclave_because_local with
     | Some { expr = mode; reasons } ->
       let rec is_ident_or_field_or_constant { pexp_desc; pexp_loc; _ } =
         match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc:pexp_loc with
         | Pexp_ident _ | Pexp_constant _ | Pexp_construct (_, None) -> true
         | Pexp_field (expr, _) | Pexp_unboxed_field (expr, _) ->
           is_ident_or_field_or_constant expr
         | _ -> false
       in
       let rec is_pure_allocation ({ pexp_desc; pexp_loc; _ } as expr) =
         match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc:pexp_loc with
         | Pexp_tuple labeled_exprs ->
           List.for_all ~f:(fun (_, expr) -> is_pure_allocation expr) labeled_exprs
         | Pexp_construct (_, None) | Pexp_variant (_, None) -> true
         | Pexp_construct (_, Some expr) | Pexp_variant (_, Some expr) ->
           is_pure_allocation expr
         | Pexp_record (fields, expr) ->
           List.for_all ~f:(fun (_, e) -> is_pure_allocation e) fields
           &&
             (match expr with
             | None -> true
             | Some expr -> is_pure_allocation expr)
         | Pexp_array (_mut, exprs) -> List.for_all ~f:is_pure_allocation exprs
         | _ -> is_ident_or_field_or_constant expr
       in
       let is_allowable =
         match expr.pexp_desc with
         | Pexp_apply (f, args) ->
           is_ident_or_field_or_constant f
           && List.for_all ~f:(fun (_, e) -> is_ident_or_field_or_constant e) args
         | _ -> is_pure_allocation expr
       in
       if is_allowable || not (List.is_empty reasons)
       then
         is_local mode ~env
         |> Result.map_error
              ~f:(Syntax_error_conversion.to_extension_node Expression expr)
       else
         Error
           ((* Ideally this check is conservative, which seems fine for now. *)
            Syntax_error.createf
              ~loc:expr.pexp_loc
              "[%%template]: exclave_if_local is only allowed on: tailcalls; syntactic \
               allocations (e.g. tuples) consisting entirely of identifiers, record \
               fields, and/or constants; or when given a nonempty ~reasons argument."
            |> Syntax_error_conversion.to_extension_node Expression expr)
     | None -> Ok false)
;;

let consume_zero_alloc_if
  (type a)
  (ctx : a Attribute_handler.Context.zero_alloc_if)
  (node : a)
  ~env
  : (a, Syntax_error.t) result
  =
  let* node, zero_alloc_if_local =
    Attribute_handler.consume Attribute_handler.zero_alloc_if_local ctx node
  in
  let* node, zero_alloc_if_stack =
    Attribute_handler.consume Attribute_handler.zero_alloc_if_stack ctx node
  in
  let* add_zero_alloc_if_local =
    match zero_alloc_if_local with
    | None -> Ok None
    | Some { loc; expr = mode; args } ->
      (match is_local mode ~env with
       | Ok true -> Ok (Some (loc, args))
       | Ok false -> Ok None
       | Error _ as err -> err)
  in
  let+ add_zero_alloc_if_stack =
    match zero_alloc_if_stack with
    | None -> Ok None
    | Some { loc; expr = alloc; args } ->
      (match is_stack alloc ~env with
       | Ok true -> Ok (Some (loc, args))
       | Ok false -> Ok None
       | Error _ as err -> err)
  in
  let add_zero_alloc =
    match add_zero_alloc_if_local, add_zero_alloc_if_stack with
    | (Some _ as some), _ | _, (Some _ as some) -> some
    | None, None -> None
  in
  match add_zero_alloc with
  | None -> node
  | Some (loc, payload) ->
    let loc = { loc with loc_ghost = true } in
    let payload =
      match payload with
      | [] -> []
      | [ expr ] -> [ Ast_builder.pstr_eval ~loc expr [] ]
      | f :: args ->
        let args = List.map args ~f:(fun arg -> Nolabel, arg) in
        [ Ast_builder.pstr_eval ~loc (Ast_builder.pexp_apply ~loc f args) [] ]
    in
    let attribute =
      Ast_builder.attribute ~loc ~name:{ txt = "zero_alloc"; loc } ~payload:(PStr payload)
    in
    (match ctx with
     | Expression -> { node with pexp_attributes = attribute :: node.pexp_attributes }
     | Value_binding -> { node with pvb_attributes = attribute :: node.pvb_attributes }
     | Value_description ->
       { node with pval_attributes = attribute :: node.pval_attributes })
;;

let t :> Context.t t =
  object (self)
    inherit [Context.t] Ppxlib_jane.Ast_traverse.map_with_context as super
    method! location ctx loc = { loc with loc_ghost = loc.loc_ghost || ctx.ghostify }

    method! jkind_annotation ctx ({ pjka_desc; pjka_loc } as jkind) =
      match pjka_desc with
      | Pjk_abbreviation ({ txt = Lident kind; _ }, sa) ->
        (match sa, Env.find ctx.env { ident = kind; type_ = Type.kind } with
         | _ :: _, Some _ ->
           Location.Error.raise
             (Location.Error.createf
                ~loc:pjka_loc
                "[ppx_template] substitution of kind variables under scannable axes is \
                 not supported")
         | _, None -> super#jkind_annotation ctx jkind
         | [], Some kind ->
           let (Jkind_annotation kind) =
             Value.to_node ~loc:(self#location ctx pjka_loc) kind
           in
           kind)
      | Pjk_mod (base, modifiers) ->
        (* Even though [modifiers] is represented as a list of [mode]s, they conceptually
           act more like modalities. *)
        let modifiers =
          List.map modifiers ~f:(fun { txt = Mode name; loc } ->
            { txt = Ppxlib_jane.Modality name; loc })
          |> self#modalities ctx
          |> List.map ~f:(fun { txt = Modality name; loc } ->
            { txt = Ppxlib_jane.Mode name; loc })
        in
        { pjka_loc = self#location ctx pjka_loc
        ; pjka_desc = Pjk_mod (self#jkind_annotation ctx base, modifiers)
        }
      | _ -> super#jkind_annotation ctx jkind

    method! modes ctx modes =
      List.map modes ~f:(fun { txt = Mode mode; loc } ->
        let loc = self#location ctx loc in
        match Env.find ctx.env { ident = mode; type_ = Type.mode } with
        | None -> { txt = Mode mode; loc }
        | Some mode ->
          let (Mode mode) = Value.to_node ~loc mode in
          mode)

    method! modalities ctx modalities =
      List.map modalities ~f:(fun { txt = Modality modality; loc } ->
        let loc = self#location ctx loc in
        match Env.find ctx.env { ident = modality; type_ = Type.modality } with
        | None -> { txt = Modality modality; loc }
        | Some modality ->
          let (Modality modality) = Value.to_node ~loc modality in
          modality)

    (* The [@kind] attribute can appear on various identifier nodes that reference values,
       modules, or types that were defined using [@kind]. For each node that could be such
       an identifier, we check if the [@kind] attribute is present, and if so, mangle that
       identifier according to the provided layouts. An error node will be created instead
       if the attribute is attached to a node which is not an identifier. *)
    method
      private visit_mono
      : type a.  a Attribute_handler.Context.mono
                -> f:(Context.t -> a -> a)
                -> Context.t
                -> a
                -> a =
      fun attr_ctx ~f ctx node ->
        (* We can't define a single [visit] function as in [visit_poly] because we need to
           call the superclass method, or else we loop infinitely, and [super] can't be
           passed around as a value (it can only be used directly with a method call). *)
        match Attribute_handler.consume Attribute_handler.mono attr_ctx node with
        | Ok (node, mangle_exprs) ->
          let node = Mangle.mangle attr_ctx node mangle_exprs ~env:ctx.env in
          f ctx node
        | Error error ->
          Syntax_error_conversion.to_extension_node
            (Attribute_handler.Context.mono_to_any attr_ctx)
            node
            error

    method! module_expr = self#visit_mono Module_expr ~f:super#module_expr
    method! core_type = self#visit_mono Core_type ~f:super#core_type

    method! module_type ctx mty =
      self#visit_mono Module_type ctx mty ~f:(fun ctx mty ->
        let mty_res =
          let* mty, with_ =
            Attribute_handler.consume Attribute_handler.with_ Module_type mty
          in
          match with_ with
          | Some with_ ->
            let with_ = self#signature ctx with_ in
            With_constraint.convert mty with_
          | None -> Ok mty
        in
        match mty_res with
        | Ok mty -> super#module_type ctx mty
        | Error err -> Syntax_error_conversion.to_extension_node Module_type mty err)

    method! expression_desc _ _ = assert false

    method! expression ctx expr =
      let loc = { expr.pexp_loc with loc_ghost = true } in
      let expr_res =
        let* expr, exclave_because_stack =
          Attribute_handler.consume Attribute_handler.exclave_if_stack Expression expr
        in
        let* expr, exclave_because_local =
          Attribute_handler.consume Attribute_handler.exclave_if_local Expression expr
        in
        let expr =
          match
            should_wrap_with_exclave
              expr
              ~exclave_because_stack
              ~exclave_because_local
              ~env:ctx.env
          with
          | Ok false -> expr
          | Ok true -> [%expr exclave_ [%e expr]]
          | Error error -> error
        in
        consume_zero_alloc_if Expression expr ~env:ctx.env
      in
      match expr_res with
      | Error err -> Syntax_error_conversion.to_extension_node Expression expr err
      | Ok expr ->
        self#visit_mono
          Expression
          ctx
          expr
          ~f:(fun ctx { pexp_attributes; pexp_loc; pexp_loc_stack; pexp_desc } ->
            let pexp_attributes = self#attributes ctx pexp_attributes in
            let pexp_loc = self#location ctx pexp_loc in
            let pexp_loc_stack = self#list self#location ctx pexp_loc_stack in
            let pexp_desc =
              let loc = pexp_loc in
              match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc with
              | Pexp_let (mutable_flag, rec_flag, bindings, expr) ->
                Nonempty_list.of_list_exn bindings
                |> self#visit_poly Value_binding ctx ~f:(fun bindings ->
                  let bindings =
                    Nonempty_list.to_list bindings |> List.map ~f:Maybe_hide_in_docs.node
                  in
                  Pexp_let
                    ( self#mutable_flag ctx mutable_flag
                    , self#rec_flag ctx rec_flag
                    , bindings
                    , self#expression ctx expr )
                  |> Ppxlib_jane.Shim.Expression_desc.to_parsetree ~loc)
              | _ -> super#expression_desc ctx pexp_desc
            in
            { pexp_attributes; pexp_loc; pexp_loc_stack; pexp_desc })

    method! value_binding ctx vb =
      match consume_zero_alloc_if Value_binding vb ~env:ctx.env with
      | Ok vb -> super#value_binding ctx vb
      | Error err -> Syntax_error_conversion.to_extension_node Value_binding vb err

    method! value_description ctx vd =
      match consume_zero_alloc_if Value_description vd ~env:ctx.env with
      | Ok vd -> super#value_description ctx vd
      | Error err -> Syntax_error_conversion.to_extension_node Value_description vd err

    (* The [@@kind] attribute can appear on various nodes that define or declare values,
       modules, or types. For each such node, we determine what layout mappings are being
       requested (no attribute is equivalent to [@@kind _ = value]), and for each such
       mapping, we duplicate the definition/declaration, and mangle the defined/declared
       name accordingly. *)
    method
      private visit_poly
      : type a
      b.  a Attribute_handler.Context.poly
         -> Context.t
         -> a Nonempty_list.t
         -> f:(a Maybe_hide_in_docs.t Nonempty_list.t -> b)
         -> b =
      fun attr_ctx ctx nodes ~f ->
        let ( (visit_self : Context.t -> a -> a)
            , (visit_mangle : Mangle.Suffix.t -> a -> a * Mangle.Outcome.t) )
          =
          match attr_ctx with
          | Value_binding -> self#value_binding, Mangle.t#value_binding
          | Value_description -> self#value_description, Mangle.t#value_description
          | Module_binding -> self#module_binding, Mangle.t#module_binding
          | Module_declaration -> self#module_declaration, Mangle.t#module_declaration
          | Type_declaration -> self#type_declaration, Mangle.t#type_declaration
          | Module_type_declaration ->
            self#module_type_declaration, Mangle.t#module_type_declaration
          | Include_infos ->
            let open Either in
            ( self#include_infos (fun ctx -> function
                | Left x -> Left (self#module_expr ctx x)
                | Right x -> Right (self#module_type ctx x))
            , Mangle.t#include_infos (fun ctx -> function
                | Left x ->
                  let x, res = Mangle.t#module_expr ctx x in
                  Left x, res
                | Right x ->
                  let x, res = Mangle.t#module_type ctx x in
                  Right x, res) )
        in
        (match consume_poly ~env:ctx.env ~defaults:ctx.defaults attr_ctx nodes with
         | Ok instances ->
           let instances =
             Nonempty_list.concat_map instances ~f:expand_explicit_plus_unmangled
           in
           Nonempty_list.mapi
             instances
             ~f:(fun i { node; env; manglers } : _ Maybe_hide_in_docs.t ->
               let suffix = Mangle.Suffix.create manglers in
               let node, res = visit_mangle suffix node in
               let node =
                 visit_self
                   { Context.ghostify = ctx.ghostify || i > 0
                   ; env
                   ; defaults = Context.Defaults.empty
                   }
                   node
               in
               { node; hide_in_docs = Mangle.Outcome.did_mangle res })
         | Error { loc = _; err; attrs = _ } ->
           let (hd_nodes :: tl_nodes) = nodes in
           [ { node =
                 Syntax_error_conversion.to_extension_node
                   (Attribute_handler.Context.poly_to_any attr_ctx)
                   hd_nodes
                   ~also_drop:tl_nodes
                   err
             ; hide_in_docs = true
             }
           ])
        |> f

    method
      private visit_floating_poly
      : type a.  a Attribute_handler.Floating.Context.poly
                -> local_ (Context.t -> a list -> a list) =
      fun attr_ctx -> exclave_
        let (visit : Context.t -> a -> a Nonempty_list.t) =
          match attr_ctx with
          | Structure_item ->
            (self#structure_item_inline
             : Context.t -> structure_item -> structure_item Nonempty_list.t)
          | Signature_item ->
            (self#signature_item_inline
             : Context.t -> signature_item -> signature_item Nonempty_list.t)
        in
        fun ctx nodes ->
          let[@tail_mod_cons] rec loop ctx (nodes : a list) =
            match nodes with
            | [] -> []
            | item :: items ->
              (* [Ppxlib] complains if we give it something besides an attribute. *)
              (match attr_ctx, item with
               | Structure_item, { pstr_desc = Pstr_attribute _; pstr_loc = attr_loc; _ }
               | Signature_item, { psig_desc = Psig_attribute _; psig_loc = attr_loc; _ }
                 ->
                 let result =
                   let* floating = Attribute_handler.Floating.convert attr_ctx item in
                   match floating with
                   | None -> Ok (Nonempty_list.to_list (visit ctx item) @ loop ctx items)
                   | Some (Define (Define bindings)) ->
                     let loc =
                       Attribute_handler.Floating.Context.location attr_ctx item
                     in
                     let original_env = ctx.env in
                     let+ env =
                       List.fold_left
                         bindings
                         ~init:(Ok original_env)
                         ~f:
                           (fun
                             env
                             ({ pattern; expression; lookup } :
                               _ Attribute_handler.Binding.t)
                           ->
                           let* env = env in
                           let* set_value = Env.eval original_env lookup expression in
                           Env.bind_set env ~loc pattern set_value)
                     in
                     loop { ctx with env } items
                   | Some (Poly { explicitness; what = { bindings; kind } }) ->
                     let* instances =
                       instantiate [ { explicitness; what = bindings } ] ~env:ctx.env
                     in
                     let is_many = Nonempty_list.length instances > 1 in
                     let+ instances =
                       List.Or_first_error.mapi
                         (Nonempty_list.to_list instances)
                         ~f:(fun i { node = (); env; manglers } ->
                           let+ defaults =
                             match kind with
                             | Never_add_mangler -> Ok ctx.defaults
                             | Add_mangler_if_more_than_one_elt when not is_many ->
                               Ok ctx.defaults
                             | Always_add_mangler | Add_mangler_if_more_than_one_elt ->
                               merge_defaults_or_error
                                 ~loc:attr_loc
                                 ~context:"merging floating defaults"
                                 ctx.defaults
                                 manglers
                           in
                           i, env, defaults)
                     in
                     List.concat_map instances ~f:(fun (i, env, defaults) ->
                       let ctx : Context.t =
                         { ghostify = ctx.ghostify || i > 0; env; defaults }
                       in
                       loop ctx items)
                 in
                 (match result with
                  | Ok items -> items
                  | Error err ->
                    Syntax_error_conversion.to_extension_node_floating
                      attr_ctx
                      ~loc:attr_loc
                      err
                    :: loop ctx items)
               | _ -> Nonempty_list.to_list (visit ctx item) @ loop ctx items)
          in
          loop ctx nodes

    method! structure ctx items = self#visit_floating_poly Structure_item ctx items
    method! signature_items ctx items = self#visit_floating_poly Signature_item ctx items
    method! structure_item _ _ = assert false
    method! structure_item_desc _ _ = assert false

    method structure_item_inline ctx =
      function
      | [%stri [%%template.portable [%%i? { pstr_desc = Pstr_module mod_; _ }]]] as stri
        ->
        Portable_stateless.module_binding Portable ~loc:stri.pstr_loc ~mod_
        |> self#structure_item_inline ctx
      | [%stri [%%template.stateless [%%i? { pstr_desc = Pstr_module mod_; _ }]]] as stri
        ->
        Portable_stateless.module_binding Stateless ~loc:stri.pstr_loc ~mod_
        |> self#structure_item_inline ctx
      | { pstr_desc = desc; pstr_loc = loc } ->
        let loc = self#location ctx loc in
        let visit_poly attr_ctx ctx nodes ~f =
          self#visit_poly attr_ctx ctx nodes ~f:(fun nodes : _ Nonempty_list.t ->
            f (Nonempty_list.map nodes ~f:Maybe_hide_in_docs.node))
        in
        (match desc with
         | Pstr_value (rec_flag, bindings) ->
           Nonempty_list.of_list_exn bindings
           |> visit_poly Value_binding ctx ~f:(fun bindings ->
             [ Ast_builder.pstr_value
                 ~loc
                 (self#rec_flag ctx rec_flag)
                 (Nonempty_list.to_list bindings)
             ])
         | Pstr_primitive desc ->
           visit_poly Value_description ctx [ desc ] ~f:(fun descs ->
             Nonempty_list.map descs ~f:(fun desc ->
               Ast_builder.pstr_primitive ~loc:desc.pval_loc desc))
         | Pstr_type (rec_flag, decls) ->
           Nonempty_list.of_list_exn decls
           |> visit_poly Type_declaration ctx ~f:(fun decls ->
             [ Ast_builder.pstr_type
                 ~loc
                 (self#rec_flag ctx rec_flag)
                 (deduplicate_deriving_attrs (Nonempty_list.to_list decls))
             ])
         | Pstr_module binding ->
           visit_poly Module_binding ctx [ binding ] ~f:(fun bindings ->
             Nonempty_list.map bindings ~f:(fun binding ->
               Ast_builder.pstr_module ~loc:binding.pmb_loc binding))
         | Pstr_recmodule bindings ->
           Nonempty_list.of_list_exn bindings
           |> visit_poly Module_binding ctx ~f:(fun bindings ->
             [ Ast_builder.pstr_recmodule ~loc (Nonempty_list.to_list bindings) ])
         | Pstr_modtype decl ->
           visit_poly Module_type_declaration ctx [ decl ] ~f:(fun decls ->
             Nonempty_list.map decls ~f:(fun decl ->
               Ast_builder.pstr_modtype ~loc:decl.pmtd_loc decl))
         | Pstr_include info ->
           visit_poly
             Include_infos
             ctx
             [ { info with pincl_mod = Left info.pincl_mod } ]
             ~f:(fun infos ->
               Nonempty_list.map infos ~f:(fun info ->
                 Ast_builder.pstr_include
                   ~loc:info.pincl_loc
                   { info with
                     pincl_mod =
                       (match info.pincl_mod with
                        | Left x -> x
                        | Right _ -> assert false)
                   }))
         | desc ->
           (* We reset the defaults here since they are supposed to act "shallowly", i.e.
              they only apply to the current layer of structure items, not nested items.
              In particular, this avoids poor interaction with [let%expect_test] items. *)
           [ { pstr_loc = loc
             ; pstr_desc =
                 super#structure_item_desc
                   { ctx with defaults = Context.Defaults.empty }
                   desc
             }
           ])

    method! signature_item _ _ = assert false
    method! signature_item_desc _ _ = assert false

    method signature_item_inline ctx =
      function
      | [%sigi: [%%template.portable: [%%i? { psig_desc = Psig_module mod_; _ }]]] as sigi
        ->
        Portable_stateless.module_declaration Portable ~loc:sigi.psig_loc ~mod_
        |> self#signature_item_inline ctx
      | [%sigi: [%%template.stateless: [%%i? { psig_desc = Psig_module mod_; _ }]]] as
        sigi ->
        Portable_stateless.module_declaration Stateless ~loc:sigi.psig_loc ~mod_
        |> self#signature_item_inline ctx
      | { psig_desc = desc; psig_loc = loc } ->
        let loc = self#location ctx loc in
        let visit_poly attr_ctx ctx item ~f =
          self#visit_poly attr_ctx ctx [ item ] ~f:(fun items ->
            let loc = { loc with loc_ghost = true } in
            Nonempty_list.to_list items
            |> List.concat_map ~f:(fun { Maybe_hide_in_docs.node = desc; hide_in_docs } ->
              let sig_ = [ f ~loc desc ] in
              if hide_in_docs then Ppx_helpers.Docs.hide ~loc sig_ else sig_)
            |> Ppx_helpers.Docs.simplify
            |> Nonempty_list.of_list_exn)
        in
        (* It is difficult to isolate individual types in e.g. a [type ... and ...] group.
           In particular, a [(**/**)] above the first item hides the entire group (and is
           not disabled by a [(**/**)] inside the group). For now, it's probably enough to
           hide the entire group if all items in it should be hidden, and to leave the
           entire group otherwise. *)
        let visit_poly_group attr_ctx ctx items ~f =
          self#visit_poly attr_ctx ctx items ~f:(fun items ->
            let items = Nonempty_list.to_list items in
            let all_hidden = List.for_all items ~f:Maybe_hide_in_docs.hide_in_docs in
            let nodes = List.map items ~f:(fun { node; hide_in_docs = _ } -> node) in
            let sigi = f nodes in
            let loc = { loc with loc_ghost = true } in
            (if all_hidden
             then Ppx_helpers.Docs.hide ~loc [ { psig_desc = sigi; psig_loc = loc } ]
             else [ { psig_desc = sigi; psig_loc = loc } ])
            |> Nonempty_list.of_list_exn)
        in
        (match Ppxlib_jane.Shim.Signature_item_desc.of_parsetree desc with
         | Psig_value desc ->
           visit_poly Value_description ctx desc ~f:Ast_builder.psig_value
         | Psig_type (rec_flag, decls) ->
           Nonempty_list.of_list_exn decls
           |> visit_poly_group Type_declaration ctx ~f:(fun decls ->
             Psig_type (self#rec_flag ctx rec_flag, deduplicate_deriving_attrs decls))
         | Psig_typesubst decls ->
           Nonempty_list.of_list_exn decls
           |> visit_poly_group Type_declaration ctx ~f:(fun decls -> Psig_typesubst decls)
         | Psig_module decl ->
           visit_poly Module_declaration ctx decl ~f:Ast_builder.psig_module
         | Psig_recmodule decls ->
           Nonempty_list.of_list_exn decls
           |> visit_poly_group Module_declaration ctx ~f:(fun decls ->
             Psig_recmodule decls)
         | Psig_modtype decl ->
           visit_poly Module_type_declaration ctx decl ~f:Ast_builder.psig_modtype
         | Psig_modtypesubst decl ->
           visit_poly Module_type_declaration ctx decl ~f:Ast_builder.psig_modtypesubst
         | Psig_include (info, moda) ->
           visit_poly
             Include_infos
             ctx
             { info with pincl_mod = Right info.pincl_mod }
             ~f:(fun ~loc info ->
               Ast_builder.psig_include
                 ~loc
                 { info with
                   pincl_mod =
                     (match info.pincl_mod with
                      | Left _ -> assert false
                      | Right x -> x)
                 }
                 ~modalities:(self#modalities ctx moda))
         | _ ->
           (* We reset the defaults here since they are supposed to act "shallowly", i.e.
              they only apply to the current layer of structure items, not nested items. *)
           [ { psig_loc = loc
             ; psig_desc =
                 super#signature_item_desc
                   { ctx with defaults = Context.Defaults.empty }
                   desc
             }
           ])
  end
;;
