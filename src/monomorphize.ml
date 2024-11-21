open! Base
open! Import

module Context = struct
  type t =
    { ghostify : bool
    ; env : Env.t
    }

  let top = { ghostify = false; env = Env.empty }
end

let extract_bindings (type a) id attr (ctx : a Attributes.Context.poly) node =
  match Attributes.consume attr ctx node with
  | None -> node, Ok (Bindings.empty id)
  | Some (node, Ok bindings) -> node, Ok bindings
  | Some (node, Error err) ->
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
    node, Error (loc, err, attrs)
;;

let extract_floating_bindings
  (type a)
  attr
  (ctx : a Attributes.Floating.Context.poly)
  item
  ~k
  =
  match Attributes.Floating.convert attr ctx item with
  | None -> Ok None
  | Some (Ok bindings) -> Ok (Some (k bindings))
  | Some (Error err) ->
    let loc =
      match ctx with
      | Structure_item -> item.pstr_loc
      | Signature_item -> item.psig_loc
    in
    Error (loc, err)
;;

let instantiate ~kinds ~modes =
  List.Cartesian_product.map2
    (Bindings.instantiate kinds)
    (Bindings.instantiate modes)
    ~f:(fun kinds modes -> Env.create ~kinds ~modes)
;;

let consume_poly (type a) (attr_ctx : a Attributes.Context.poly) nodes =
  List.concat_map nodes ~f:(fun node ->
    let node, kinds =
      extract_bindings Identifier.kind Attributes.kind_poly attr_ctx node
    in
    let node, modes =
      extract_bindings Identifier.mode Attributes.mode_poly attr_ctx node
    in
    match kinds, modes with
    | (Error _ as err), Ok _ | Ok _, (Error _ as err) -> [ err ]
    | (Error _ as err1), (Error _ as err2) -> [ err1; err2 ]
    | Ok kinds, Ok modes ->
      let instances = instantiate ~kinds ~modes in
      let kinds = Bindings.vars kinds in
      let modes = Bindings.vars modes in
      [ Ok
          (List.map instances ~f:(fun env ->
             let kinds = List.map ~f:(Env.find_kind_exn env) kinds in
             let modes = List.map ~f:(Env.find_mode_exn env) modes in
             node, env, kinds, modes))
      ])
  |> Result.combine_errors
  |> function
  | Ok instances -> Ok (List.concat instances)
  | Error errs ->
    let locs, errs, attrs = List.unzip3 errs in
    let loc =
      List.reduce_exn locs ~f:(fun a b ->
        { loc_start = Location.min_pos a.loc_start b.loc_start
        ; loc_end = Location.max_pos a.loc_end b.loc_end
        ; loc_ghost = a.loc_ghost || b.loc_ghost
        })
    in
    let err = Error.of_list errs in
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
  [%sigi: include [%m Ast_builder.pmty_signature ~loc (List.map nodes ~f:(f ~loc))]]
    .psig_desc
;;

let error_to_string_hum ~loc err =
  Location.error_extensionf
    ~loc
    "%s"
    (Error.to_string_hum (Error.tag err ~tag:"[%template]"))
;;

let t =
  object (self)
    inherit [Context.t] Ppxlib_jane.Ast_traverse.map_with_context as super
    method! location ctx loc = { loc with loc_ghost = loc.loc_ghost || ctx.ghostify }

    method! jkind_annotation ctx ({ pjkind_desc; pjkind_loc } as jkind) =
      match pjkind_desc with
      | Abbreviation kind ->
        (match Env.find_kind ctx.env (Identifier.Kind.of_string kind) with
         | None -> super#jkind_annotation ctx jkind
         | Some kind -> Binding.Kind.to_node ~loc:pjkind_loc kind)
      | _ -> super#jkind_annotation ctx jkind

    method! modes ctx modes =
      List.map modes ~f:(fun { txt = Mode mode; loc } ->
        let loc = self#location ctx loc in
        match Env.find_mode ctx.env (Identifier.Mode.of_string mode) with
        | None -> { txt = Mode mode; loc }
        | Some mode -> { txt = Binding.Mode.to_node ~loc mode; loc })

    (* The [@kind] attribute can appear on various identifier nodes that reference values,
       modules,or types that were defined using [@kind]. For each node that could be such
       an identifier, we check if the [@kind] attribute is present, and if so, mangle that
       identifier according to the provided layouts.  An error node will be created
       instead if the attribute is attached to a node which is not an identifier. *)
    method private visit_mono : type a. a Attributes.Context.mono -> Context.t -> a -> a =
      fun attr_ctx ctx node ->
        (* We can't define a single [visit] function as in [visit_poly] because we need to
           call the superclass method, or else we loop infinitely, and [super] can't be
           passed around as a value (it can only be used directly with a method call). *)
        let node, kinds =
          Attributes.consume Attributes.kind_mono attr_ctx node
          |> Option.value ~default:(node, [])
        in
        let node, modes =
          Attributes.consume Attributes.mode_mono attr_ctx node
          |> Option.value ~default:(node, [])
        in
        let node =
          match kinds, modes with
          | [], [] -> node
          | _ ->
            let suffix = Mangle.Suffix.create ~env:ctx.env ~kinds ~modes in
            (match attr_ctx with
             | Expression -> Mangle.t#expression suffix node
             | Module_expr -> Mangle.t#module_expr suffix node
             | Core_type -> Mangle.t#core_type suffix node
             | Module_type -> Mangle.t#module_type suffix node)
        in
        match attr_ctx with
        | Expression -> super#expression ctx node
        | Module_expr -> super#module_expr ctx node
        | Core_type -> super#core_type ctx node
        | Module_type -> super#module_type ctx node

    method! module_expr = self#visit_mono Module_expr
    method! core_type = self#visit_mono Core_type
    method! module_type = self#visit_mono Module_type

    method! expression ctx expr =
      let expr =
        match Attributes.consume Attributes.exclave_if_local Expression expr with
        | None -> expr
        | Some (expr, mode) ->
          let rec is_ident_or_field_or_constant = function
            | { pexp_desc = Pexp_ident _ | Pexp_constant _; _ } -> true
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
            | Pexp_construct (_, expr) | Pexp_variant (_, expr) ->
              Option.for_all ~f:is_pure_allocation expr
            | Pexp_record (fields, expr) ->
              List.for_all ~f:(snd >> is_pure_allocation) fields
              && Option.for_all ~f:is_pure_allocation expr
            | Pexp_array (_mut, exprs) -> List.for_all ~f:is_pure_allocation exprs
            | _ -> is_ident_or_field_or_constant expr
          in
          let is_allowable =
            match expr.pexp_desc with
            | Pexp_apply (f, args) ->
              is_ident_or_field_or_constant f
              && List.for_all ~f:(snd >> is_ident_or_field_or_constant) args
            | _ -> is_pure_allocation expr
          in
          let loc = { expr.pexp_loc with loc_ghost = true } in
          if is_allowable
          then
            if Identifier.Mode.equal
                 (Identifier.Mode.of_string "local")
                 (Env.find_mode ctx.env mode |> Option.value ~default:mode)
            then [%expr [%e expr]]
            else expr
          else
            (* Ideally this check is conservative, which seems fine for now. *)
            Ast_builder.pexp_extension
              ~loc
              (error_to_string_hum
                 ~loc
                 (Error.of_string
                    "exclave_if_local is only allowed on tailcalls or syntactic \
                     allocations (e.g. tuples) consisting entirely of identifiers, \
                     record fields, and/or constants"))
      in
      self#visit_mono Expression ctx expr

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
        let visit (type ctx) (visitor : ctx Ast_traverse.map_with_context) : ctx -> a -> a
          =
          match attr_ctx with
          | Value_binding -> visitor#value_binding
          | Value_description -> visitor#value_description
          | Module_binding -> visitor#module_binding
          | Module_declaration -> visitor#module_declaration
          | Type_declaration -> visitor#type_declaration
          | Module_type_declaration -> visitor#module_type_declaration
          | Include_infos ->
            visitor#include_infos (fun ctx ->
              Either.map
                ~first:(visitor#module_expr ctx)
                ~second:(visitor#module_type ctx))
        in
        match consume_poly attr_ctx nodes with
        | Ok instances ->
          List.mapi instances ~f:(fun i (node, env, kinds, modes) ->
            let env =
              Env.lookup_and_set_all ~current_env:ctx.env ~uninterpreted_env:env
            in
            visit
              self
              { Context.ghostify = ctx.ghostify || i > 0; env }
              (visit Mangle.t (Mangle.Suffix.create ~env ~kinds ~modes) node))
          |> f
        | Error (loc, err, attrs) -> on_error (error_to_string_hum ~loc err) attrs

    method
      private visit_floating_poly
      : type a. a Attributes.Floating.Context.poly -> Context.t -> a list -> a list =
      fun attr_ctx ->
        let ( (visit : Context.t -> a -> a)
            , (on_error : loc:location -> extension -> attributes -> a) )
          =
          match attr_ctx with
          | Structure_item -> self#structure_item, Ast_builder.pstr_extension
          | Signature_item -> self#signature_item, Ast_builder.psig_extension
        in
        let[@tail_mod_cons] rec loop ~k ctx : a list -> a list = function
          | [] -> continue k
          | item :: items ->
            (* [Ppxlib] complains if we give it something besides an attribute. *)
            (match attr_ctx, item with
             | Structure_item, { pstr_desc = Pstr_attribute _; _ }
             | Signature_item, { psig_desc = Psig_attribute _; _ } ->
               let bindings =
                 match
                   extract_floating_bindings
                     Attributes.Floating.kind_poly
                     attr_ctx
                     item
                     ~k:(fun kinds -> kinds, Bindings.empty Identifier.mode)
                 with
                 | (Error _ | Ok (Some _)) as res -> res
                 | Ok None ->
                   extract_floating_bindings
                     Attributes.Floating.mode_poly
                     attr_ctx
                     item
                     ~k:(fun modes -> Bindings.empty Identifier.kind, modes)
               in
               (match bindings with
                | Ok None -> visit ctx item :: loop ~k ctx items
                | Ok (Some (kinds, modes)) ->
                  let[@tail_mod_cons] rec mapi_append i xs ys ~f =
                    match xs with
                    | [] -> ys
                    | x :: xs -> f i x :: mapi_append (i + 1) xs ys ~f
                  in
                  mapi_append 0 (instantiate ~kinds ~modes) k ~f:(fun i env ->
                    ( { Context.ghostify = ctx.ghostify || i > 0
                      ; env =
                          Env.lookup_and_set_all
                            ~current_env:ctx.env
                            ~uninterpreted_env:env
                      }
                    , items ))
                  |> continue
                | Error (loc, err) ->
                  on_error ~loc (error_to_string_hum ~loc err) [] :: loop ~k ctx items)
             | _ -> visit ctx item :: loop ~k ctx items)
        and[@tail_mod_cons] continue = function
          | [] -> []
          | (ctx, items) :: k -> loop ~k ctx items
        in
        loop ~k:[]

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
            ~f:(fun decls -> Pstr_type (self#rec_flag ctx rec_flag, decls))
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
            [ { info with pincl_mod = First info.pincl_mod } ]
            ~f:(fun infos ->
              List.map infos ~f:(fun info ->
                { info with
                  pincl_mod =
                    Either.value_map info.pincl_mod ~first:Fn.id ~second:(fun _ ->
                      assert false)
                })
              |> function
              | [ info ] -> Pstr_include info
              | infos ->
                include_struct ~loc:info.pincl_loc infos ~f:Ast_builder.pstr_include)
            ~on_error
        | desc -> super#structure_item_desc ctx desc

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
            ~f:(fun decls -> Psig_type (self#rec_flag ctx rec_flag, decls))
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
            [ { info with pincl_mod = Second info.pincl_mod } ]
            ~f:(fun infos ->
              List.map infos ~f:(fun info ->
                { info with
                  pincl_mod =
                    Either.value_map
                      info.pincl_mod
                      ~first:(fun _ -> assert false)
                      ~second:Fn.id
                })
              |> function
              | [ info ] ->
                Ppxlib_jane.Shim.Signature_item_desc.to_parsetree
                  (Psig_include (info, moda))
              | infos -> include_sig ~loc:info.pincl_loc infos ~f:Ast_builder.psig_include)
            ~on_error
        | _ -> super#signature_item_desc ctx desc
  end
;;
