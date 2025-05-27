open! Stdppx
open! Import
open Language
include Attributes_intf.Definitions

module type Context = sig
  type ('a, 'w) t : immediate
  type 'w packed = T : (_, 'w) t -> 'w packed [@@unboxed]
end

module type Attribute = sig
  module Context : sig
    type 'a t
  end

  type ('a, 'b) t
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

module Make
    (Attribute : sig
       include Attribute

       val declare
         :  string
         -> 'a Context.t
         -> (payload, 'k, 'b) Ast_pattern.t
         -> 'k
         -> ('a, 'b) t
     end)
    (Context : sig
       include Context

       val to_ppxlib : ('a, _) t -> 'a Attribute.Context.t
       val same_witness_exn : ('a, 'w) t -> ('b, 'w) t -> ('a, 'b) Stdlib.Type.eq
     end) =
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

  type 'a poly =
    ( 'a
      , [ `value_binding
        | `value_description
        | `module_binding
        | `module_declaration
        | `type_declaration
        | `module_type_declaration
        | `include_infos
        ] )
      t

  type 'a mono = ('a, [ `expression | `module_expr | `core_type | `module_type ]) t
  type 'a zero_alloc_if = ('a, [ `expression | `value_binding | `value_description ]) t

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
end

include Make (Attribute) (Context)

let consume_attr attr ctx ast = Attribute.consume (Attribute_map.find_exn attr ctx) ast

type ('w, 'b) t = { f : 'a. ('a, 'w) Context.t -> 'a -> 'a * 'b } [@@unboxed]

let consume t ctx item = t.f ctx item

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

  let basic_pattern type_ expression_pat =
    Ast_pattern_helpers.binding_poly
      ~pattern_pat:(Ast_pattern_helpers.ident_pattern type_)
      ~expression_pat
      ~mangle:(fun value -> value)
      ~mangle_type:type_
  ;;

  let declare_basic type_ expr_pat ~name =
    declare ~name ~contexts ~pattern:(basic_pattern type_ expr_pat) ~k:Fn.id
  ;;

  let kind_attr = declare_basic Type.kind Ast_pattern_helpers.kind_expr ~name:"kind"

  let mode_attr =
    declare_basic Type.mode (Ast_pattern_helpers.ident_expr Type.mode) ~name:"mode"
  ;;

  let modality_attr =
    declare_basic
      Type.modality
      (Ast_pattern_helpers.ident_expr Type.modality)
      ~name:"modality"
  ;;

  let alloc_binding_pattern () =
    Ast_pattern.( ||| )
      (* e.g. [[@@alloc a = (heap, stack)]] *)
      (Ast_pattern_helpers.binding_poly
         ~pattern_pat:(Ast_pattern_helpers.ident_pattern Type.alloc)
         ~expression_pat:(Ast_pattern_helpers.ident_expr Type.alloc)
         ~mangle:(fun alloc -> alloc)
         ~mangle_type:Type.alloc)
      (* e.g. [[@@alloc a @ m = (heap_global, stack @ local)]] *)
      (Ast_pattern_helpers.binding_poly
         ~pattern_pat:Ast_pattern_helpers.alloc_pattern
         ~expression_pat:Ast_pattern_helpers.alloc_expr
         ~mangle:(fun (Tuple2 (alloc, _)) -> alloc)
         ~mangle_type:Type.alloc)
  ;;

  let alloc_attr =
    declare ~name:"alloc" ~contexts ~pattern:(alloc_binding_pattern ()) ~k:Fn.id
  ;;

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

  let validate (Poly (_, bindings)) =
    let duplicate_expression_errors =
      List.filter_map bindings ~f:(fun { pattern; expressions; _ } ->
        match
          find_all_dups expressions ~compare:(fun e1 e2 ->
            Expression.compare e1.txt e2.txt)
        with
        | [] -> None
        | dups ->
          Some
            (Sexp.message
               "duplicate expressions for single pattern"
               [ ( ""
                 , List
                     [ Pattern.sexp_of_t pattern
                     ; sexp_of_list Expression.sexp_of_t (List.map ~f:Loc.txt dups)
                     ] )
               ]))
    in
    let duplicate_pattern_errors =
      match
        find_all_dups
          bindings
          ~compare:(fun { pattern = pat1; _ } { pattern = pat2; _ } ->
            Pattern.compare pat1 pat2)
      with
      | [] -> []
      | dups ->
        let pats = List.map dups ~f:(fun { pattern; _ } -> Pattern.sexp_of_t pattern) in
        [ Sexp.message "duplicate patterns" [ "", List pats ] ]
    in
    match duplicate_expression_errors @ duplicate_pattern_errors with
    | [] -> Ok ()
    | [ error ] -> Error error
    | errors -> Error (List errors)
  ;;
end

let consume_poly ctx item =
  let consume attr item =
    match consume_attr attr ctx item with
    | None -> item, []
    | Some (item, bindings) -> item, [ bindings ]
  in
  let item, kinds = consume Poly.kind_attr item in
  let item, modes = consume Poly.mode_attr item in
  let item, modalities = consume Poly.modality_attr item in
  let item, allocs = consume Poly.alloc_attr item in
  let polys = kinds @ modes @ modalities @ allocs in
  let polys_res =
    polys
    |> List.map ~f:Poly.validate
    |> List.filter_map ~f:(function
      | Ok () -> None
      | Error err -> Some err)
    |> function
    | [] -> Ok polys
    | [ err ] -> Error err
    | errs -> Error (List errs)
  in
  item, polys_res
;;

let poly = { f = consume_poly }

module Mono = struct
  type t = Expression.Basic.packed Loc.t list Type.Map.t

  let contexts : _ Context.packed list =
    [ T Expression; T Module_expr; T Core_type; T Module_type ]
  ;;

  let declare expr_pat ~name =
    declare ~name ~contexts ~pattern:(Ast_pattern_helpers.binding_mono expr_pat) ~k:Fn.id
  ;;

  let kind_attr = declare Ast_pattern_helpers.kind_expr ~name:"kind"
  let mode_attr = declare (Ast_pattern_helpers.ident_expr Type.mode) ~name:"mode"

  let modality_attr =
    declare (Ast_pattern_helpers.ident_expr Type.modality) ~name:"modality"
  ;;

  let alloc_attr = declare (Ast_pattern_helpers.ident_expr Type.alloc) ~name:"alloc"
end

let consume_mono ctx item =
  let consume mono type_ attr item =
    match consume_attr attr ctx item with
    | None -> item, mono
    | Some (item, vals) -> item, Type.Map.add (P type_) vals mono
  in
  let mono = Type.Map.empty in
  let item, mono = consume mono Type.kind Mono.kind_attr item in
  let item, mono = consume mono Type.mode Mono.mode_attr item in
  let item, mono = consume mono Type.modality Mono.modality_attr item in
  let item, mono = consume mono Type.alloc Mono.alloc_attr item in
  item, mono
;;

let mono = { f = consume_mono }

let consume_attr_if attr =
  { f =
      (fun ctx item ->
        match consume_attr attr ctx item with
        | None -> item, None
        | Some (item, expr) -> item, Some expr)
  }
;;

module Exclave_if = struct
  let declare ~name ~type_ =
    declare
      ~name
      ~contexts:[ T Expression ]
      ~pattern:
        (Ast_pattern.single_expr_payload ((Ast_pattern_helpers.ident_expr type_).pat ()))
      ~k:Fn.id
  ;;

  let local_attr = declare ~name:"exclave_if_local" ~type_:Type.mode
  let stack_attr = declare ~name:"exclave_if_stack" ~type_:Type.alloc
end

let exclave_if_local = consume_attr_if Exclave_if.local_attr
let exclave_if_stack = consume_attr_if Exclave_if.stack_attr

module Zero_alloc_if = struct
  let pattern type_ =
    let open Ast_pattern in
    single_expr_payload
      (map ((Ast_pattern_helpers.ident_expr type_).pat ()) ~f:(fun k mode -> k mode [])
       ||| pexp_apply
             ((Ast_pattern_helpers.ident_expr type_).pat ())
             (many (pair nolabel __)))
    |> map2' ~f:(fun loc mode payload -> loc, mode, payload)
  ;;

  let declare ~name ~type_ =
    declare
      ~name
      ~contexts:[ T Expression; T Value_binding; T Value_description ]
      ~pattern:(pattern type_)
      ~k:Fn.id
  ;;

  let local_attr = declare ~name:"zero_alloc_if_local" ~type_:Type.mode
  let stack_attr = declare ~name:"zero_alloc_if_stack" ~type_:Type.alloc
end

let zero_alloc_if_local = consume_attr_if Zero_alloc_if.local_attr
let zero_alloc_if_stack = consume_attr_if Zero_alloc_if.stack_attr

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

  include Make (Attribute.Floating) (Context)

  let convert_attrs attrs ctx ast =
    Attribute.Floating.convert
      (List.map attrs ~f:(fun attr -> Attribute_map.find_exn attr ctx))
      ast
  ;;

  module Attached_poly = Poly

  module Poly = struct
    type t =
      { bindings : Attached_poly.t
      ; default : bool
      }

    let validate t = Attached_poly.validate t.bindings
  end

  let contexts : _ Context.packed list = [ T Structure_item; T Signature_item ]

  let declare_poly ~name ~pattern =
    List.map [ false; true ] ~f:(fun default ->
      let name = if default then name ^ ".default" else name in
      declare ~name ~contexts ~pattern ~k:(fun bindings -> { Poly.bindings; default }))
  ;;

  let declare_basic type_ expr_pat ~name =
    declare_poly ~name ~pattern:(Attached_poly.basic_pattern type_ expr_pat)
  ;;

  let kind_poly = declare_basic Type.kind Ast_pattern_helpers.kind_expr ~name:"kind"

  let mode_poly =
    declare_basic Type.mode (Ast_pattern_helpers.ident_expr Type.mode) ~name:"mode"
  ;;

  let modality_poly =
    declare_basic
      Type.modality
      (Ast_pattern_helpers.ident_expr Type.modality)
      ~name:"modality"
  ;;

  let alloc_poly =
    declare_poly ~name:"alloc" ~pattern:(Attached_poly.alloc_binding_pattern ())
  ;;

  let convert_poly ctx ast =
    convert_attrs (kind_poly @ mode_poly @ modality_poly @ alloc_poly) ctx ast
    |> Option.map ~f:(fun poly -> poly |> Poly.validate |> Result.map ~f:(fun () -> poly))
  ;;
end
