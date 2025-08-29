open! Stdppx
open! Import
open Language
include Attributes_intf.Definitions
open Result.Let_syntax
module Type = Language.Typed.Type

module type Context = sig
  type ('a, 'w) t [@@immediate]
  type 'w packed = T : (_, 'w) t -> 'w packed [@@unboxed]
end

module type Attribute = sig
  module Context : sig
    type 'a t
  end

  type ('a, 'b) t
end

module With_attribute_maybe_explicit (Context : Context) (Attribute : Attribute) = struct
  type ('w, 'b) t =
    | T : ('a, 'w) Context.t * ('a, 'b) Attribute.t Maybe_explicit.Both.t -> ('w, 'b) t
end

module With_attribute (Context : Context) (Attribute : Attribute) = struct
  type ('w, 'b) t = T : ('a, 'w) Context.t * ('a, 'b) Attribute.t -> ('w, 'b) t
end

module Map_poly : sig
  type ('key, 'data) t

  val find_exn : ('key, 'data) t -> 'key -> 'data
  val of_list : ('key * 'data) list -> ('key, 'data) t
end = struct
  module Key = struct
    type t = Poly : _ -> t

    let compare = Stdppx.Poly.compare
  end

  module Map = Map.Make (Key)

  type (_, 'data) t = 'data Map.t

  let find_exn t key = Map.find (Poly key) t

  let of_list list =
    Map.of_list (List.map list ~f:(fun (key, data) -> Key.Poly key, data))
  ;;
end

module type Attribute_arg = sig
  include Attribute

  val declare
    :  string
    -> 'a Context.t
    -> (payload, 'k, 'b) Ast_pattern.t
    -> 'k
    -> ('a, 'b) t
end

module type Context_arg = sig
  module Attribute : Attribute_arg
  include Context

  val to_ppxlib : ('a, _) t -> 'a Attribute.Context.t
  val same_witness_exn : ('a, 'w) t -> ('b, 'w) t -> ('a, 'b) Stdlib.Type.eq
end

module Make_maybe_explicit
    (Attribute : Attribute_arg)
    (Context : Context_arg with module Attribute := Attribute) =
struct
  module Attribute_map = struct
    type ('w, 'b) t =
      ( 'w Context.packed
        , ('w, 'b) With_attribute_maybe_explicit(Context)(Attribute).t )
        Map_poly.t

    let find_exn (type a w b) (t : (w, b) t) (ctx : (a, w) Context.t)
      : (a, b) Attribute.t Maybe_explicit.Both.t
      =
      let (T (ctx', attribute)) = Map_poly.find_exn t (T ctx) in
      let Equal = Context.same_witness_exn ctx ctx' in
      attribute
    ;;
  end

  let declare ~name ~contexts ~pattern ~k =
    Map_poly.of_list
      (List.map contexts ~f:(fun (T context as key : _ Context.packed) ->
         let attribute =
           Maybe_explicit.Both.create (fun explicit ->
             let name =
               match explicit with
               | Explicit -> name ^ ".explicit"
               | Drop_axis_if_all_defaults -> name
             in
             Attribute.declare ("template." ^ name) (Context.to_ppxlib context) pattern k)
         in
         ( key
         , (T (context, attribute)
            : _ With_attribute_maybe_explicit(Context)(Attribute).t) )))
  ;;
end

module Make
    (Attribute : Attribute_arg)
    (Context : Context_arg with module Attribute := Attribute) =
struct
  module Attribute_map = struct
    type ('w, 'b) t =
      ('w Context.packed, ('w, 'b) With_attribute(Context)(Attribute).t) Map_poly.t

    let find_exn (type a w b) (t : (w, b) t) (ctx : (a, w) Context.t) : (a, b) Attribute.t
      =
      let (T (ctx', attribute)) = Map_poly.find_exn t (T ctx) in
      let Equal = Context.same_witness_exn ctx ctx' in
      attribute
    ;;
  end

  let declare ~name ~contexts ~pattern ~k =
    Map_poly.of_list
      (List.map contexts ~f:(fun (T context as key : _ Context.packed) ->
         let attribute =
           Attribute.declare ("template." ^ name) (Context.to_ppxlib context) pattern k
         in
         key, (T (context, attribute) : _ With_attribute(Context)(Attribute).t)))
  ;;
end

module Context = struct
  include Context

  type 'w packed = T : (_, 'w) t -> 'w packed [@@unboxed]

  let poly_to_any : type a. a poly -> a any = function
    | Value_binding -> Value_binding
    | Value_description -> Value_description
    | Module_binding -> Module_binding
    | Module_declaration -> Module_declaration
    | Type_declaration -> Type_declaration
    | Module_type_declaration -> Module_type_declaration
    | Include_infos -> Include_infos
  ;;

  let mono_to_any : type a. a mono -> a any = function
    | Expression -> Expression
    | Module_expr -> Module_expr
    | Core_type -> Core_type
    | Module_type -> Module_type
  ;;

  let zero_alloc_if_to_any : type a. a zero_alloc_if -> a any = function
    | Expression -> Expression
    | Value_binding -> Value_binding
    | Value_description -> Value_description
  ;;

  let to_ppxlib : type a w. (a, w) t -> a Attribute.Context.t = function
    | Expression -> Expression
    | Module_expr -> Module_expr
    | Core_type -> Core_type
    | Module_type -> Module_type
    | Value_binding -> Value_binding
    | Value_description -> Value_description
    | Module_binding -> Module_binding
    | Module_declaration -> Module_declaration
    | Type_declaration -> Type_declaration
    | Module_type_declaration -> Module_type_declaration
    | Include_infos -> Include_infos
  ;;

  let same_witness_exn (type a b w) (a : (a, w) t) (b : (b, w) t) : (a, b) Stdlib.Type.eq =
    match a, b with
    | Expression, Expression -> Equal
    | Module_expr, Module_expr -> Equal
    | Core_type, Core_type -> Equal
    | Module_type, Module_type -> Equal
    | Value_binding, Value_binding -> Equal
    | Value_description, Value_description -> Equal
    | Module_binding, Module_binding -> Equal
    | Module_declaration, Module_declaration -> Equal
    | Type_declaration, Type_declaration -> Equal
    | Module_type_declaration, Module_type_declaration -> Equal
    | Include_infos, Include_infos -> Equal
    | ( Expression
      , ( Module_expr
        | Core_type
        | Module_type
        | Value_binding
        | Value_description
        | Module_binding
        | Module_declaration
        | Type_declaration
        | Module_type_declaration
        | Include_infos ) )
    | ( Module_expr
      , ( Expression
        | Core_type
        | Module_type
        | Value_binding
        | Value_description
        | Module_binding
        | Module_declaration
        | Type_declaration
        | Module_type_declaration
        | Include_infos ) )
    | ( Core_type
      , ( Expression
        | Module_expr
        | Module_type
        | Value_binding
        | Value_description
        | Module_binding
        | Module_declaration
        | Type_declaration
        | Module_type_declaration
        | Include_infos ) )
    | ( Module_type
      , ( Expression
        | Module_expr
        | Core_type
        | Value_binding
        | Value_description
        | Module_binding
        | Module_declaration
        | Type_declaration
        | Module_type_declaration
        | Include_infos ) )
    | ( Value_binding
      , ( Expression
        | Module_expr
        | Core_type
        | Module_type
        | Value_description
        | Module_binding
        | Module_declaration
        | Type_declaration
        | Module_type_declaration
        | Include_infos ) )
    | ( Value_description
      , ( Expression
        | Module_expr
        | Core_type
        | Module_type
        | Value_binding
        | Module_binding
        | Module_declaration
        | Type_declaration
        | Module_type_declaration
        | Include_infos ) )
    | ( Module_binding
      , ( Expression
        | Module_expr
        | Core_type
        | Module_type
        | Value_binding
        | Value_description
        | Module_declaration
        | Type_declaration
        | Module_type_declaration
        | Include_infos ) )
    | ( Module_declaration
      , ( Expression
        | Module_expr
        | Core_type
        | Module_type
        | Value_binding
        | Value_description
        | Module_binding
        | Type_declaration
        | Module_type_declaration
        | Include_infos ) )
    | ( Type_declaration
      , ( Expression
        | Module_expr
        | Core_type
        | Module_type
        | Value_binding
        | Value_description
        | Module_binding
        | Module_declaration
        | Module_type_declaration
        | Include_infos ) )
    | ( Module_type_declaration
      , ( Expression
        | Module_expr
        | Core_type
        | Module_type
        | Value_binding
        | Value_description
        | Module_binding
        | Module_declaration
        | Type_declaration
        | Include_infos ) )
    | ( Include_infos
      , ( Expression
        | Module_expr
        | Core_type
        | Module_type
        | Value_binding
        | Value_description
        | Module_binding
        | Module_declaration
        | Type_declaration
        | Module_type_declaration ) ) -> assert false
  ;;

  let location : type a b. (a, b) t -> a -> Location.t = function
    | Core_type -> fun x -> x.ptyp_loc
    | Expression -> fun x -> x.pexp_loc
    | Module_expr -> fun x -> x.pmod_loc
    | Module_type -> fun x -> x.pmty_loc
    | Value_binding -> fun x -> x.pvb_loc
    | Value_description -> fun x -> x.pval_loc
    | Module_binding -> fun x -> x.pmb_loc
    | Module_declaration -> fun x -> x.pmd_loc
    | Type_declaration -> fun x -> x.ptype_loc
    | Module_type_declaration -> fun x -> x.pmtd_loc
    | Include_infos -> fun x -> x.pincl_loc
  ;;
end

include Make_maybe_explicit (Attribute) (Context)

let raise_you_can_only_use_one_attribute_per_axis ~loc =
  Location.raise_errorf
    ~loc
    "You cannot have two attributes for the same axis.\n\
     E.g. you cannot have [let f = ... [@@mode.explicit x] [@@mode y]]."
;;

let consume_attr attr ctx ast =
  Maybe_explicit.Both.opt_fold_map
    (Attribute_map.find_exn attr ctx)
    ~init:ast
    ~f:(fun ast attr ->
      match Attribute.consume attr ast with
      | None -> ast, None
      | Some (ast, res) -> ast, Some res)
  |> function
  | _, Neither -> None
  | ast, One res -> Some (ast, res)
  | _, Both _ ->
    let loc = Context.location ctx ast in
    raise_you_can_only_use_one_attribute_per_axis ~loc
;;

type ('w, 'b) t = { f : 'a. ('a, 'w) Context.t -> 'a -> 'a * 'b } [@@unboxed]

let consume t ctx item = t.f ctx item

let consume_result t ctx item =
  match t.f ctx item with
  | item, Ok value -> Ok (item, value)
  | _item, (Error _ as err) -> err
;;

module Poly = struct
  include Poly

  let contexts : _ Context.packed list =
    [ T Value_binding
    ; T Value_description
    ; T Module_binding
    ; T Module_declaration
    ; T Type_declaration
    ; T Module_type_declaration
    ; T Include_infos
    ]
  ;;

  let declare name =
    declare ~name ~contexts ~pattern:(Ast_pattern_helpers.bindings ()) ~k:Fn.id
  ;;

  let kind_attr = declare "kind"
  let mode_attr = declare "mode"
  let modality_attr = declare "modality"
  let alloc_attr = declare "alloc"

  let find_all_dups (type a) list ~compare =
    let module Set =
      Set.Make (struct
        type t = a

        let compare = compare
      end)
    in
    let _, dups =
      List.fold_left list ~init:(Set.empty, Set.empty) ~f:(fun (seen, dups) elt ->
        if Set.mem elt dups
        then seen, dups
        else if Set.mem elt seen
        then seen, Set.add elt dups
        else Set.add elt seen, dups)
    in
    Set.to_list dups
  ;;

  let validate_bindings bindings =
    let duplicate_expression_errors =
      List.map bindings ~f:(fun (pattern, expressions) ->
        match
          find_all_dups expressions ~compare:(fun e1 e2 ->
            Untyped.Expression.compare e1.txt e2.txt)
        with
        | [] -> Ok ()
        | dups ->
          Error
            (Sexp.message
               "duplicate expressions for single pattern"
               [ ( ""
                 , List
                     [ Untyped.Pattern.sexp_of_t pattern
                     ; sexp_of_list
                         Untyped.Expression.sexp_of_t
                         (List.map ~f:Loc.txt dups)
                     ] )
               ]))
    in
    let duplicate_pattern_error =
      match
        find_all_dups bindings ~compare:(fun (pat1, _) (pat2, _) ->
          Untyped.Pattern.compare pat1 pat2)
      with
      | [] -> Ok ()
      | dups ->
        let pats =
          List.map dups ~f:(fun (pattern, _) -> Untyped.Pattern.sexp_of_t pattern)
        in
        Error (Sexp.message "duplicate patterns" [ "", List pats ])
    in
    let+ (_ : unit list) =
      duplicate_pattern_error :: duplicate_expression_errors |> Result.collect_errors
    in
    ()
  ;;

  let type_check_one (pattern, expressions) ~expected ~mangle_axis ~mangle =
    let* pattern = Typed.Pattern.type_check pattern ~expected in
    let+ expressions =
      List.map expressions ~f:(fun { txt = expr; loc } ->
        let+ expr = Typed.Expression.type_check expr ~expected in
        { txt = expr; loc })
      |> Result.all
    in
    Poly.Binding { pattern; expressions; mangle_axis; mangle }
  ;;

  let type_check_many bindings ~expected ~mangle_axis ~mangle =
    let+ bindings =
      List.map bindings ~f:(fun binding ->
        type_check_one binding ~expected ~mangle_axis ~mangle)
      |> Result.all
    in
    Poly (mangle_axis, bindings)
  ;;

  let type_check_many_maybe_tuple
    (type expected)
    bindings
    ~(expected : expected Type.basic Type.t)
    ~mangle_axis
    ~mangle
    =
    let+ bindings =
      List.map bindings ~f:(fun ((pattern, _) as binding) ->
        (* Determine whether the binding is a tuple binding via the shape of [pattern] *)
        match (pattern : Untyped.Pattern.t) with
        | Tuple patterns ->
          (* Calculate the tuple type based on the shape of [patterns] *)
          let open struct
            type 'a nonempty_tuple = P : ('a * _) Type.tuple -> 'a nonempty_tuple
          end in
          let rec loop : _ list -> expected Type.basic nonempty_tuple = function
            | [] -> assert false
            | [ _ ] -> P [ expected ]
            | _ :: tl ->
              let (P tl) = loop tl in
              P (expected :: tl)
          in
          let (P tuple) = loop patterns in
          let expected = Type.Tuple tuple in
          let mangle (Typed.Value.Tuple (hd :: _)) = hd in
          type_check_one binding ~expected ~mangle_axis ~mangle
        | _ ->
          (* If the pattern is just an identifier, assume it's not a tuple binding *)
          type_check_one binding ~expected ~mangle_axis ~mangle)
      |> Result.all
    in
    Poly (mangle_axis, bindings)
  ;;
end

let consume_poly ctx item =
  let consume attr item =
    match consume_attr attr ctx item with
    | None -> item, None
    | Some (item, bindings) -> item, Some bindings
  in
  let type_check_opt type_check bindings ~expected ~mangle_axis ~mangle =
    Option.map bindings ~f:(fun bindings ->
      Maybe_explicit.map_result bindings ~f:(fun bindings ->
        type_check bindings ~expected ~mangle_axis ~mangle))
  in
  let item, kinds = consume Poly.kind_attr item in
  let item, modes = consume Poly.mode_attr item in
  let item, modalities = consume Poly.modality_attr item in
  let item, allocs = consume Poly.alloc_attr item in
  let res =
    let untyped = [ kinds; modes; modalities; allocs ] |> List.filter_opt in
    let* typed =
      let kinds =
        type_check_opt
          Poly.type_check_many_maybe_tuple
          kinds
          ~expected:Type.kind
          ~mangle_axis:Kind
          ~mangle:Fn.id
      in
      let modes =
        type_check_opt
          Poly.type_check_many_maybe_tuple
          modes
          ~expected:Type.mode
          ~mangle_axis:Mode
          ~mangle:Fn.id
      in
      let modalities =
        type_check_opt
          Poly.type_check_many_maybe_tuple
          modalities
          ~expected:Type.modality
          ~mangle_axis:Modality
          ~mangle:Fn.id
      in
      let allocs =
        match
          type_check_opt
            Poly.type_check_many
            allocs
            ~expected:Type.alloc
            ~mangle_axis:Alloc
            ~mangle:Fn.id
        with
        | (Some (Ok _) | None) as res -> res
        | Some (Error _) ->
          type_check_opt
            Poly.type_check_many
            allocs
            ~expected:Type.(tuple2 alloc mode)
            ~mangle_axis:Alloc
            ~mangle:(fun (Tuple [ alloc; _mode ]) -> alloc)
      in
      [ kinds; modes; modalities; allocs ]
      |> List.filter_opt
      |> List.map ~f:Type_error.lift_to_error_result
      |> Result.collect_errors
    in
    let+ (_ : unit list) =
      untyped
      |> List.map ~f:(fun (_explicitness, binding) -> binding |> Poly.validate_bindings)
      |> Result.collect_errors
    in
    typed
  in
  item, res
;;

let poly = { f = consume_poly }

module Mono = struct
  include Mono

  let contexts : _ Context.packed list =
    [ T Expression; T Module_expr; T Core_type; T Module_type ]
  ;;

  let declare name expected =
    declare
      ~name
      ~contexts
      ~pattern:(Ast_pattern_helpers.multiple_idents ())
      ~k:(fun exprs ->
        List.map exprs ~f:(fun { txt = expr; loc } ->
          Result.map (Typed.Expression.type_check expr ~expected) ~f:(fun expr ->
            Loc.make ~loc (Typed.Expression.Basic.P expr)))
        |> List.map ~f:Type_error.lift_to_error_result
        |> Result.collect_errors)
  ;;

  let kind_attr = declare "kind" Type.kind
  let mode_attr = declare "mode" Type.mode
  let modality_attr = declare "modality" Type.modality
  let alloc_attr = declare "alloc" Type.alloc
end

let consume_mono ctx item =
  let consume mono axis attr item =
    match consume_attr attr ctx item with
    | None -> item, mono
    | Some (item, vals) ->
      ( item
      , let* mono = mono in
        let+ vals = Maybe_explicit.ok vals in
        Typed.Axis.Map.add (P axis) vals mono )
  in
  let mono = Ok Typed.Axis.Map.empty in
  let item, mono = consume mono Kind Mono.kind_attr item in
  let item, mono = consume mono Mode Mono.mode_attr item in
  let item, mono = consume mono Modality Mono.modality_attr item in
  let item, mono = consume mono Alloc Mono.alloc_attr item in
  item, mono
;;

let mono = { f = consume_mono }

module Floating = struct
  module Context = struct
    type ('a, 'w) t =
      | Structure_item : (structure_item, [> `structure_item ]) t
      | Signature_item : (signature_item, [> `signature_item ]) t

    type 'w packed = T : (_, 'w) t -> 'w packed [@@unboxed]
    type 'a poly = ('a, [ `structure_item | `signature_item ]) t

    let to_ppxlib : type a w. (a, w) t -> a Attribute.Floating.Context.t = function
      | Structure_item -> Structure_item
      | Signature_item -> Signature_item
    ;;

    let same_witness_exn (type a b w) (a : (a, w) t) (b : (b, w) t)
      : (a, b) Stdlib.Type.eq
      =
      match a, b with
      | Structure_item, Structure_item -> Equal
      | Signature_item, Signature_item -> Equal
      | Structure_item, Signature_item | Signature_item, Structure_item -> assert false
    ;;
  end

  include Make_maybe_explicit (Attribute.Floating) (Context)

  let convert_attrs attrs ctx ast =
    attrs
    |> List.map ~f:(fun attr -> Attribute_map.find_exn attr ctx)
    |> Maybe_explicit.Both.all
    |> Maybe_explicit.Both.opt_map ~f:(fun attr -> Attribute.Floating.convert attr ast)
    |> function
    | Neither -> None
    | One res -> Some (Maybe_explicit.ok res)
    | Both _ -> raise_you_can_only_use_one_attribute_per_axis ~loc:Location.none
  ;;

  module Attached_poly = Poly

  module Poly = struct
    type t =
      { bindings : Attached_poly.t
      ; default : bool
      }
  end

  let contexts : _ Context.packed list = [ T Structure_item; T Signature_item ]

  let declare name type_check =
    List.map [ false; true ] ~f:(fun default ->
      let name = if default then name ^ ".default" else name in
      declare
        ~name
        ~contexts
        ~pattern:(Ast_pattern_helpers.bindings ())
        ~k:(fun bindings ->
          let* () = Attached_poly.validate_bindings bindings in
          let+ bindings = type_check bindings |> Type_error.lift_to_error_result in
          ({ bindings; default } : Poly.t)))
  ;;

  let kind_poly =
    declare "kind" (fun bindings ->
      Attached_poly.type_check_many_maybe_tuple
        bindings
        ~expected:Type.kind
        ~mangle_axis:Kind
        ~mangle:Fn.id)
  ;;

  let mode_poly =
    declare "mode" (fun bindings ->
      Attached_poly.type_check_many_maybe_tuple
        bindings
        ~expected:Type.mode
        ~mangle_axis:Mode
        ~mangle:Fn.id)
  ;;

  let modality_poly =
    declare "modality" (fun bindings ->
      Attached_poly.type_check_many_maybe_tuple
        bindings
        ~expected:Type.modality
        ~mangle_axis:Modality
        ~mangle:Fn.id)
  ;;

  let alloc_poly =
    declare "alloc" (fun bindings ->
      match
        Attached_poly.type_check_many
          bindings
          ~expected:Type.alloc
          ~mangle_axis:Alloc
          ~mangle:Fn.id
      with
      | Ok _ as ok -> ok
      | Error _ ->
        Attached_poly.type_check_many
          bindings
          ~expected:Type.(tuple2 alloc mode)
          ~mangle_axis:Alloc
          ~mangle:(fun (Tuple [ alloc; _mode ]) -> alloc))
  ;;

  let all_attrs = kind_poly @ mode_poly @ modality_poly @ alloc_poly

  let is_present (type a) (ctx : a Context.poly) (ast : a) =
    let maybe_attr =
      match ctx, ast with
      | Signature_item, { psig_desc = Psig_attribute attr; _ } -> Some attr
      | Signature_item, _ -> None
      | Structure_item, { pstr_desc = Pstr_attribute attr; _ } -> Some attr
      | Structure_item, _ -> None
    in
    match maybe_attr with
    | None -> false
    | Some attr ->
      let ast_attr_name =
        let name = attr.attr_name.txt in
        if String.is_prefix name ~prefix:"template." then name else "template." ^ name
      in
      List.concat_map all_attrs ~f:(fun attr ->
        Attribute_map.find_exn attr ctx
        |> Maybe_explicit.Both.extract_list
        |> List.map ~f:Attribute.Floating.name)
      |> List.exists ~f:(fun attr_name -> String.equal attr_name ast_attr_name)
  ;;

  let convert_poly ctx ast =
    match convert_attrs all_attrs ctx ast with
    | None -> Ok None
    | Some (Ok value) -> Ok (Some value)
    | Some (Error _ as err) -> err
  ;;
end

module Non_explicit = struct
  (* We don't want to shadow from [Make_maybe_explicit] *)
  open Make (Attribute) (Context)

  let consume_attr_if attr =
    let consume_attr attr ctx ast =
      Attribute.consume (Attribute_map.find_exn attr ctx) ast
    in
    { f =
        (fun ctx item ->
          match consume_attr attr ctx item with
          | None -> item, Ok None
          | Some (item, Ok value) -> item, Ok (Some value)
          | Some (item, (Error _ as err)) -> item, err)
    }
  ;;

  module Exclave_if = struct
    let declare name expected =
      declare
        ~name
        ~contexts:[ T Expression ]
        ~pattern:(Ast_pattern_helpers.single_ident ())
        ~k:(fun { txt = expr; loc } ->
          (let+ expr = Typed.Expression.type_check expr ~expected in
           { txt = expr; loc })
          |> Type_error.lift_to_error_result)
    ;;

    let local_attr = declare "exclave_if_local" Type.mode
    let stack_attr = declare "exclave_if_stack" Type.alloc
  end

  let exclave_if_local = consume_attr_if Exclave_if.local_attr
  let exclave_if_stack = consume_attr_if Exclave_if.stack_attr

  module Zero_alloc_if = struct
    let pattern () =
      let open Ast_pattern in
      single_expr_payload
        (map (Ast_pattern_helpers.ident_expr ()) ~f:(fun k mode -> k mode [])
         ||| pexp_apply (Ast_pattern_helpers.ident_expr ()) (many (pair nolabel __)))
      |> map2' ~f:(fun loc mode payload -> loc, mode, payload)
    ;;

    let declare name expected =
      declare
        ~name
        ~contexts:[ T Expression; T Value_binding; T Value_description ]
        ~pattern:(pattern ())
        ~k:(fun (loc, { txt = expr; loc = mode_loc }, args) ->
          (let+ expr = Typed.Expression.type_check expr ~expected in
           loc, { txt = expr; loc = mode_loc }, args)
          |> Type_error.lift_to_error_result)
    ;;

    let local_attr = declare "zero_alloc_if_local" Type.mode
    let stack_attr = declare "zero_alloc_if_stack" Type.alloc
  end

  let zero_alloc_if_local = consume_attr_if Zero_alloc_if.local_attr
  let zero_alloc_if_stack = consume_attr_if Zero_alloc_if.stack_attr
end

include Non_explicit
