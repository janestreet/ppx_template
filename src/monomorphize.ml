open! Stdppx
open! Import
open Language

module Context = struct
  module Defaults = struct
    type t = Value.Basic.packed list Type.Map.t

    let empty = Type.Map.empty
  end

  type t =
    { ghostify : bool
    ; env : Env.t
    ; defaults : Defaults.t
    }

  let top = { ghostify = false; env = Language.Env.initial; defaults = Defaults.empty }
end

let extract_bindings (type a) (ctx : a Attributes.Context.poly) node =
  match Attributes.consume Attributes.poly ctx node with
  | node, Ok bindings -> Ok (node, bindings)
  | node, Error err ->
    let loc, attrs =
      match ctx with
      | Value_binding -> node.pvb_loc, node.pvb_attributes
      | Value_description -> node.pval_loc, node.pval_attributes
      | Module_binding -> node.pmb_loc, node.pmb_attributes
      | Module_declaration -> node.pmd_loc, node.pmd_attributes
      | Type_declaration -> node.ptype_loc, node.ptype_attributes
      | Module_type_declaration -> node.pmtd_loc, node.pmtd_attributes
      | Include_infos -> node.pincl_loc, node.pincl_attributes
    in
    Error (loc, err, attrs)
;;

let extract_floating_bindings (type a) (ctx : a Attributes.Floating.Context.poly) item =
  match Attributes.Floating.convert_poly ctx item with
  | None -> Ok None
  | Some (Ok poly) -> Ok (Some poly)
  | Some (Error err) ->
    let loc =
      match ctx with
      | Structure_item -> item.pstr_loc
      | Signature_item -> item.psig_loc
    in
    Error (loc, err)
;;

let stable_dedup list ~compare =
  List.mapi list ~f:(fun i x -> i, x)
  |> List.sort_uniq ~cmp:(fun (_, x) (_, y) -> compare x y)
  |> List.sort ~cmp:(fun (i, _) (j, _) -> Int.compare i j)
  |> List.map ~f:snd
;;

let handle_one_binding ~original_env envs ({ pattern; expressions; mangle } : _ Binding.t)
  =
  List.concat_map envs ~f:(fun (env, manglers) ->
    List.map expressions ~f:(fun expr -> Env.eval original_env expr)
    (* If multiple expressions evaluate to the same value, only generate one instance. *)
    |> stable_dedup ~compare:Value.compare
    |> List.map ~f:(fun value ->
      let env = Env.bind env pattern value in
      let mangle_value = mangle value in
      let mangle_type = Value.type_ mangle_value in
      let manglers =
        Type.Map.add_to_list (P mangle_type) (Value.Basic.P mangle_value) manglers
      in
      env, manglers))
;;

let handle_one_attribute ~original_env envs (Poly (type_, bindings) : Attributes.Poly.t) =
  (* Explicitly set the entry in the manglers for the current [type_] to empty so that
     putting an empty [[@@kind]] attribute disables the default kind mangling from
     [[@@@kind.default]]. *)
  let init =
    List.map envs ~f:(fun (env, manglers) -> env, Type.Map.add (P type_) [] manglers)
  in
  List.fold_left bindings ~init ~f:(handle_one_binding ~original_env)
;;

let instantiate poly_attributes ~env:(original_env : Env.t) =
  let envs =
    List.fold_left
      poly_attributes
      ~init:[ original_env, Context.Defaults.empty ]
      ~f:(handle_one_attribute ~original_env)
  in
  List.map envs ~f:(fun (env, manglers) ->
    (* Manglers were added in reverse order *)
    env, Type.Map.map List.rev manglers)
;;

let consume_poly
  (type a)
  ~env
  ~(defaults : Context.Defaults.t)
  (attr_ctx : a Attributes.Context.poly)
  nodes
  =
  List.map nodes ~f:(fun node ->
    match extract_bindings attr_ctx node with
    | Error _ as err -> err
    | Ok (node, polys) ->
      let instances =
        List.map (instantiate polys ~env) ~f:(fun (env, manglers) ->
          (* Use current default manglers when axis is unspecified. *)
          let manglers =
            Type.Map.merge
              (fun _ defaults manglers ->
                match defaults, manglers with
                | None, None -> None
                | _, Some _ -> manglers
                | Some _, None -> defaults)
              defaults
              manglers
          in
          (* Associate node with the instance *)
          node, env, manglers)
      in
      Ok instances)
  |> List.fold_right ~init:(Ok []) ~f:(fun head tail ->
    match head, tail with
    | Ok head, Ok tail -> Ok (head :: tail)
    | Error head, Error tail -> Error (head :: tail)
    | Error head, Ok _ -> Error [ head ]
    | Ok _, Error tail -> Error tail)
  |> function
  | Ok instances -> Ok (List.concat instances)
  | Error errs ->
    let locs, errs, attrs =
      List.fold_right
        errs
        ~init:([], [], [])
        ~f:(fun (loc, err, attr) (locs, errs, attrs) ->
          loc :: locs, err :: errs, attr :: attrs)
    in
    let loc =
      List.fold_left ~init:(List.hd locs) (List.tl locs) ~f:(fun a b ->
        { loc_start = Location.min_pos a.loc_start b.loc_start
        ; loc_end = Location.max_pos a.loc_end b.loc_end
        ; loc_ghost = a.loc_ghost || b.loc_ghost
        })
    in
    let err =
      match errs with
      | _ :: _ :: _ -> Sexp.message "multiple errors" [ "", List errs ]
      | [ err ] -> err
      | [] -> Atom "empty error list"
    in
    let attrs = List.concat attrs in
    Error (loc, err, attrs)
;;

let include_struct ~loc nodes ~f =
  let loc = { loc with loc_ghost = true } in
  [%stri include [%m Ast_builder.pmod_structure ~loc (List.map nodes ~f:(f ~loc))]]
    .pstr_desc
;;

let include_sig ~loc nodes ~f =
  let loc = { loc with loc_ghost = true } in
  [%sigi:
    include
      [%m
    Ast_builder.pmty_signature
      ~loc
      (Ast_builder.signature ~loc (List.map nodes ~f:(f ~loc)))]]
    .psig_desc
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

let explicitly_drop = Attribute.explicitly_drop

let error_to_string_hum ~loc err explicitly_drop_method node =
  explicitly_drop_method node;
  Location.error_extensionf
    ~loc
    "%s"
    (Sexp.to_string_hum (Sexp.message "[%template]" [ "", err ]))
;;

let is_local (mode : Type.mode Expression.t Loc.t) ~env =
  let (Identifier ident) = Env.eval env mode in
  match ident.ident with
  | "local" -> true
  | "global" -> false
  | ident ->
    Location.raise_errorf ~loc:mode.loc "Unknown or invalid mode identifier: %s" ident
;;

let is_stack (alloc : Type.alloc Expression.t Loc.t) ~env =
  let (Identifier ident) = Env.eval env alloc in
  match ident.ident with
  | "stack" -> true
  | "heap" -> false
  | ident ->
    Location.raise_errorf ~loc:alloc.loc "Unbound allocation identifier: %s" ident
;;

let consume_zero_alloc_if
  (type a)
  (ctx : a Attributes.Context.zero_alloc_if)
  (node : a)
  ~env
  =
  let node, zero_alloc_if_local =
    Attributes.consume Attributes.zero_alloc_if_local ctx node
  in
  let node, zero_alloc_if_stack =
    Attributes.consume Attributes.zero_alloc_if_stack ctx node
  in
  let add_zero_alloc =
    match zero_alloc_if_local, zero_alloc_if_stack with
    | Some (loc, mode, payload), _ when is_local mode ~env -> Some (loc, payload)
    | _, Some (loc, alloc, payload) when is_stack alloc ~env -> Some (loc, payload)
    | _, _ -> None
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

let t =
  object (self)
    inherit [Context.t] Ppxlib_jane.Ast_traverse.map_with_context as super
    method! location ctx loc = { loc with loc_ghost = loc.loc_ghost || ctx.ghostify }

    method! jkind_annotation ctx ({ pjkind_desc; pjkind_loc } as jkind) =
      match pjkind_desc with
      | Abbreviation kind ->
        (match Env.find ctx.env { ident = kind; type_ = Type.kind } with
         | None -> super#jkind_annotation ctx jkind
         | Some kind ->
           let (Jkind_annotation kind) = Value.to_node ~loc:pjkind_loc kind in
           kind)
      | Mod (base, modifiers) ->
        (* Even though [modifiers] is represented as a list of [mode]s, they conceptually
           act more like modalities. *)
        let modifiers =
          List.map modifiers ~f:(fun { txt = Mode name; loc } ->
            { txt = Ppxlib_jane.Modality name; loc })
          |> self#modalities ctx
          |> List.map ~f:(fun { txt = Modality name; loc } ->
            { txt = Ppxlib_jane.Mode name; loc })
        in
        { pjkind_loc = self#location ctx pjkind_loc
        ; pjkind_desc = Mod (self#jkind_annotation ctx base, modifiers)
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
       identifier according to the provided layouts.  An error node will be created
       instead if the attribute is attached to a node which is not an identifier. *)
    method private visit_mono : type a. a Attributes.Context.mono -> Context.t -> a -> a =
      fun attr_ctx ctx node ->
        (* We can't define a single [visit] function as in [visit_poly] because we need to
           call the superclass method, or else we loop infinitely, and [super] can't be
           passed around as a value (it can only be used directly with a method call). *)
        let node, mangle_exprs = Attributes.consume Attributes.mono attr_ctx node in
        let node = Mangle.mangle attr_ctx node mangle_exprs ~env:ctx.env in
        match attr_ctx with
        | Expression -> super#expression ctx node
        | Module_expr -> super#module_expr ctx node
        | Core_type -> super#core_type ctx node
        | Module_type -> super#module_type ctx node

    method! module_expr = self#visit_mono Module_expr
    method! core_type = self#visit_mono Core_type
    method! module_type = self#visit_mono Module_type

    method! expression ctx expr =
      let loc = { expr.pexp_loc with loc_ghost = true } in
      let expr =
        let expr, exclave_because_stack =
          Attributes.consume Attributes.exclave_if_stack Expression expr
        in
        let expr, exclave_because_local =
          Attributes.consume Attributes.exclave_if_local Expression expr
        in
        match exclave_because_stack, exclave_because_local with
        | Some alloc, _ when is_stack alloc ~env:ctx.env -> [%expr exclave_ [%e expr]]
        | _, Some mode ->
          let rec is_ident_or_field_or_constant = function
            | { pexp_desc = Pexp_ident _ | Pexp_constant _ | Pexp_construct (_, None); _ }
              -> true
            | { pexp_desc = Pexp_field (expr, _); _ } ->
              is_ident_or_field_or_constant expr
            | _ -> false
          in
          let rec is_pure_allocation ({ pexp_desc; pexp_loc; _ } as expr) =
            match
              Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc:pexp_loc
            with
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
          if is_allowable
          then if is_local mode ~env:ctx.env then [%expr exclave_ [%e expr]] else expr
          else
            (* Ideally this check is conservative, which seems fine for now. *)
            Ast_builder.pexp_extension
              ~loc
              (error_to_string_hum
                 ~loc
                 (Sexp.Atom
                    "exclave_if_local is only allowed on tailcalls or syntactic \
                     allocations (e.g. tuples) consisting entirely of identifiers, \
                     record fields, and/or constants")
                 explicitly_drop#expression
                 expr)
        | _, _ -> expr
      in
      self#visit_mono Expression ctx (consume_zero_alloc_if Expression expr ~env:ctx.env)

    method! value_binding ctx vb =
      super#value_binding ctx (consume_zero_alloc_if Value_binding vb ~env:ctx.env)

    method! value_description ctx vd =
      super#value_description
        ctx
        (consume_zero_alloc_if Value_description vd ~env:ctx.env)

    (* The [@@kind] attribute can appear on various nodes that define or declare values,
       modules, or types. For each such node, we determine what layout mappings are being
       requested (no attribute is equivalent to [@@kind __ = value]), and for each
       such mapping, we duplicate the definition/declaration, and mangle the
       defined/declared name accordingly. *)
    method
      private visit_poly
      : type a
      b.  a Attributes.Context.poly
         -> Context.t
         -> a list
         -> f:(a list -> b)
         -> on_error:(extension -> attributes -> b)
         -> b =
      fun attr_ctx ctx nodes ~f ~on_error ->
        let ( (visit_self : Context.t -> a -> a)
            , (visit_mangle : Mangle.Suffix.t -> a -> a)
            , (drop : a -> unit) )
          =
          match attr_ctx with
          | Value_binding ->
            self#value_binding, Mangle.t#value_binding, explicitly_drop#value_binding
          | Value_description ->
            ( self#value_description
            , Mangle.t#value_description
            , explicitly_drop#value_description )
          | Module_binding ->
            self#module_binding, Mangle.t#module_binding, explicitly_drop#module_binding
          | Module_declaration ->
            ( self#module_declaration
            , Mangle.t#module_declaration
            , explicitly_drop#module_declaration )
          | Type_declaration ->
            ( self#type_declaration
            , Mangle.t#type_declaration
            , explicitly_drop#type_declaration )
          | Module_type_declaration ->
            ( self#module_type_declaration
            , Mangle.t#module_type_declaration
            , explicitly_drop#module_type_declaration )
          | Include_infos ->
            let open Either in
            ( self#include_infos (fun ctx -> function
                | Left x -> Left (self#module_expr ctx x)
                | Right x -> Right (self#module_type ctx x))
            , Mangle.t#include_infos (fun ctx -> function
                | Left x -> Left (Mangle.t#module_expr ctx x)
                | Right x -> Right (Mangle.t#module_type ctx x))
            , explicitly_drop#include_infos (function
                | Left x -> explicitly_drop#module_expr x
                | Right x -> explicitly_drop#module_type x) )
        in
        match consume_poly ~env:ctx.env ~defaults:ctx.defaults attr_ctx nodes with
        | Ok instances ->
          List.mapi instances ~f:(fun i (node, env, manglers) ->
            visit_self
              { Context.ghostify = ctx.ghostify || i > 0
              ; env
              ; defaults = Context.Defaults.empty
              }
              (visit_mangle (Mangle.Suffix.create manglers) node))
          |> f
        | Error (loc, err, attrs) ->
          on_error (error_to_string_hum ~loc err (List.iter ~f:drop) nodes) attrs

    method
      private visit_floating_poly
      : type a.  a Attributes.Floating.Context.poly
                -> local_ (Context.t -> a list -> a list) =
      fun attr_ctx -> exclave_
        let ( (visit : Context.t -> a -> a)
            , (on_error : loc:location -> extension -> attributes -> a)
            , (drop : a -> unit)
            , (wrap_include : loc:location -> a list -> a) )
          =
          match attr_ctx with
          | Structure_item ->
            let include_ ~loc items =
              [%stri include [%m Ast_builder.pmod_structure ~loc items]]
            in
            ( self#structure_item
            , Ast_builder.pstr_extension
            , explicitly_drop#structure_item
            , include_ )
          | Signature_item ->
            let include_ ~loc items =
              [%sigi:
                include
                  [%m
                Ast_builder.pmty_signature ~loc (Ast_builder.signature ~loc items)]]
            in
            ( self#signature_item
            , Ast_builder.psig_extension
            , explicitly_drop#signature_item
            , include_ )
        in
        let[@tail_mod_cons] rec loop ctx (nodes : a list) =
          match nodes with
          | [] -> []
          | item :: items ->
            (* [Ppxlib] complains if we give it something besides an attribute. *)
            (match attr_ctx, item with
             | Structure_item, { pstr_desc = Pstr_attribute _; pstr_loc = loc; _ }
             | Signature_item, { psig_desc = Psig_attribute _; psig_loc = loc; _ } ->
               (match extract_floating_bindings attr_ctx item with
                | Ok None -> visit ctx item :: loop ctx items
                | Ok (Some { bindings; default }) ->
                  let instances = instantiate [ bindings ] ~env:ctx.env in
                  List.mapi instances ~f:(fun i (env, manglers) ->
                    let defaults =
                      match default with
                      | false -> ctx.defaults
                      | true ->
                        Type.Map.merge
                          (fun _ ctx_defaults new_defaults ->
                            match ctx_defaults, new_defaults with
                            | None, None -> None
                            | Some defaults, None | None, Some defaults -> Some defaults
                            | Some ctx_defaults, Some new_defaults ->
                              Some (ctx_defaults @ new_defaults))
                          ctx.defaults
                          manglers
                    in
                    let ctx : Context.t =
                      { ghostify = ctx.ghostify || i > 0; env; defaults }
                    in
                    wrap_include ~loc:{ loc with loc_ghost = true } (loop ctx items))
                | Error (loc, err) ->
                  on_error ~loc (error_to_string_hum ~loc err drop item) []
                  :: loop ctx items)
             | _ -> visit ctx item :: loop ctx items)
        in
        loop

    method! structure ctx items = self#visit_floating_poly Structure_item ctx items
    method! signature_items ctx items = self#visit_floating_poly Signature_item ctx items

    method! expression_desc =
      let on_error err _ = Pexp_extension err in
      fun ctx -> function
        | Pexp_let (rec_flag, bindings, expr) ->
          self#visit_poly
            Value_binding
            ctx
            bindings
            ~f:(fun bindings ->
              Pexp_let (self#rec_flag ctx rec_flag, bindings, self#expression ctx expr))
            ~on_error
        | desc -> super#expression_desc ctx desc

    method! structure_item ctx =
      function
      | [%stri [%%template.portable [%%i? { pstr_desc = Pstr_module mod_; _ }]]] as stri
        -> Portable.module_binding ~loc:stri.pstr_loc mod_ |> self#structure_item ctx
      | stri -> super#structure_item ctx stri

    method! structure_item_desc =
      let on_error err attrs = Pstr_extension (err, attrs) in
      fun ctx -> function
        | Pstr_value (rec_flag, bindings) ->
          self#visit_poly
            Value_binding
            ctx
            bindings
            ~f:(fun bindings -> Pstr_value (self#rec_flag ctx rec_flag, bindings))
            ~on_error
        | Pstr_primitive desc ->
          self#visit_poly
            Value_description
            ctx
            [ desc ]
            ~f:(function
              | [ desc ] -> Pstr_primitive desc
              | descs ->
                include_struct ~loc:desc.pval_loc descs ~f:Ast_builder.pstr_primitive)
            ~on_error
        | Pstr_type (rec_flag, decls) ->
          self#visit_poly
            Type_declaration
            ctx
            decls
            ~f:(fun decls ->
              Pstr_type (self#rec_flag ctx rec_flag, deduplicate_deriving_attrs decls))
            ~on_error
        | Pstr_module binding ->
          self#visit_poly
            Module_binding
            ctx
            [ binding ]
            ~f:(function
              | [ binding ] -> Pstr_module binding
              | bindings ->
                include_struct ~loc:binding.pmb_loc bindings ~f:Ast_builder.pstr_module)
            ~on_error
        | Pstr_recmodule bindings ->
          self#visit_poly
            Module_binding
            ctx
            bindings
            ~f:(fun bindings -> Pstr_recmodule bindings)
            ~on_error
        | Pstr_modtype decl ->
          self#visit_poly
            Module_type_declaration
            ctx
            [ decl ]
            ~f:(function
              | [ decl ] -> Pstr_modtype decl
              | decls ->
                include_struct ~loc:decl.pmtd_loc decls ~f:Ast_builder.pstr_modtype)
            ~on_error
        | Pstr_include info ->
          self#visit_poly
            Include_infos
            ctx
            [ { info with pincl_mod = Left info.pincl_mod } ]
            ~f:(fun infos ->
              List.map infos ~f:(fun info ->
                { info with
                  pincl_mod =
                    (match info.pincl_mod with
                     | Left x -> x
                     | Right _ -> assert false)
                })
              |> function
              | [ info ] -> Pstr_include info
              | infos ->
                include_struct ~loc:info.pincl_loc infos ~f:Ast_builder.pstr_include)
            ~on_error
        | desc ->
          (* We reset the defaults here since they are supposed to act "shallowly", i.e.
             they only apply to the current layer of structure items, not nested items.
             In particular, this avoids poor interaction with [let%expect_test] items. *)
          super#structure_item_desc { ctx with defaults = Context.Defaults.empty } desc

    method! signature_item ctx =
      function
      | [%sigi: [%%template.portable: [%%i? { psig_desc = Psig_module mod_; _ }]]] as sigi
        -> Portable.module_declaration ~loc:sigi.psig_loc mod_ |> self#signature_item ctx
      | sigi -> super#signature_item ctx sigi

    method! signature_item_desc =
      let on_error err attrs = Psig_extension (err, attrs) in
      fun ctx desc ->
        match Ppxlib_jane.Shim.Signature_item_desc.of_parsetree desc with
        | Psig_value desc ->
          self#visit_poly
            Value_description
            ctx
            [ desc ]
            ~f:(function
              | [ desc ] -> Psig_value desc
              | descs -> include_sig ~loc:desc.pval_loc descs ~f:Ast_builder.psig_value)
            ~on_error
        | Psig_type (rec_flag, decls) ->
          self#visit_poly
            Type_declaration
            ctx
            decls
            ~f:(fun decls ->
              Psig_type (self#rec_flag ctx rec_flag, deduplicate_deriving_attrs decls))
            ~on_error
        | Psig_typesubst decls ->
          self#visit_poly
            Type_declaration
            ctx
            decls
            ~f:(fun decls -> Psig_typesubst decls)
            ~on_error
        | Psig_module decl ->
          self#visit_poly
            Module_declaration
            ctx
            [ decl ]
            ~f:(function
              | [ decl ] -> Psig_module decl
              | decls -> include_sig ~loc:decl.pmd_loc decls ~f:Ast_builder.psig_module)
            ~on_error
        | Psig_recmodule decls ->
          self#visit_poly
            Module_declaration
            ctx
            decls
            ~f:(fun decls -> Psig_recmodule decls)
            ~on_error
        | Psig_modtype decl ->
          self#visit_poly
            Module_type_declaration
            ctx
            [ decl ]
            ~f:(function
              | [ decl ] -> Psig_modtype decl
              | decls -> include_sig ~loc:decl.pmtd_loc decls ~f:Ast_builder.psig_modtype)
            ~on_error
        | Psig_modtypesubst decl ->
          self#visit_poly
            Module_type_declaration
            ctx
            [ decl ]
            ~f:(function
              | [ decl ] -> Psig_modtypesubst decl
              | decls ->
                include_sig ~loc:decl.pmtd_loc decls ~f:Ast_builder.psig_modtypesubst)
            ~on_error
        | Psig_include (info, moda) ->
          self#visit_poly
            Include_infos
            ctx
            [ { info with pincl_mod = Right info.pincl_mod } ]
            ~f:(fun infos ->
              List.map infos ~f:(fun info ->
                { info with
                  pincl_mod =
                    (match info.pincl_mod with
                     | Left _ -> assert false
                     | Right x -> x)
                })
              |> function
              | [ info ] ->
                Ppxlib_jane.Shim.Signature_item_desc.to_parsetree
                  (Psig_include (info, self#modalities ctx moda))
              | infos ->
                include_sig
                  ~loc:info.pincl_loc
                  infos
                  ~f:(Ast_builder.psig_include ~modalities:[]))
            ~on_error
        | _ ->
          (* We reset the defaults here since they are supposed to act "shallowly", i.e.
             they only apply to the current layer of structure items, not nested items. *)
          super#signature_item_desc { ctx with defaults = Context.Defaults.empty } desc
  end
;;
