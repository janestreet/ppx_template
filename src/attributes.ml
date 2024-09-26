open! Base
open! Import

let at_most_one_pattern p = Ast_pattern.(p ^:: nil ||| map0 nil ~f:[])
let at_most_one_eval p = Ast_pattern.(pstr (at_most_one_pattern (pstr_eval p nil)))

let ident (type a cmp) ((module Id) : (a, cmp) Identifier.t) =
  Ast_pattern.(pexp_ident (map1 (lident __) ~f:Id.of_string))
;;

let many_or_one a b = Ast_pattern.(a ||| map1 b ~f:List.return)
let tuple_or_one p = many_or_one Ast_pattern.(pexp_tuple (many p)) p

(* Parses an [expression] of the form [a b c] (i.e. function application of identifiers)
   as [[ "a"; "b"; "c" ]]. *)
let ident_apply_pattern id =
  Ast_pattern.(
    many_or_one
      (map2 (pexp_apply (ident id) (many (pair nolabel (ident id)))) ~f:List.cons)
      (ident id))
;;

(* Parses an [expression] of the form [a, b, c] (i.e. a tuple of identifiers) as
   [[ "a"; "b"; "c" ]]. *)
let ident_tuple_pattern id = ident id |> tuple_or_one

(* Parses an [expression] of the form [l = (a, b)] as ["l", [ "a"; "b" ]]. *)
let ident_equals_pattern id =
  Ast_pattern.(
    pexp_apply
      (pexp_ident (lident (string "=")))
      (pair nolabel (ident id) ^:: pair nolabel (ident_tuple_pattern id) ^:: nil)
    |> pack2)
;;

(* Parses a [payload] of the form [l = (a, b), m = (c, d)] as
   [[ "l", [ "a"; "b" ]; "m", [ "c"; "d" ] ]], or of the form [a b] as
   [[ "a", [ "a" ]; "b", [ "b" ]]]. *)
let ident_poly_pattern id =
  Ast_pattern.(
    tuple_or_one (ident_equals_pattern id)
    ||| map1 (ident_apply_pattern id) ~f:(List.map ~f:(fun jkind -> jkind, [ jkind ]))
    |> at_most_one_eval)
;;

(* Parses a [payload] of the form [a b c] as [[ "a"; "b"; "c" ]]. *)
let ident_mono_pattern id = ident_apply_pattern id |> at_most_one_eval

module type Context = sig
  type ('a, 'w) t [@@immediate]
end

module type Attribute = sig
  module Context : sig
    type 'a t
  end

  type ('a, 'b) t
end

module Packed (Context : Context) = struct
  type 'w t = T : (_, 'w) Context.t -> 'w t [@@unboxed]
end

module With_attribute (Context : Context) (Attribute : Attribute) = struct
  type ('w, 'b) t = T : ('a, 'w) Context.t * ('a, 'b) Attribute.t -> ('w, 'b) t
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
       val same_witness_exn : ('a, 'w) t -> ('b, 'w) t -> ('a, 'b) Type_equal.t
     end) =
struct
  type ('w, 'b) t =
    ('w Packed(Context).t, ('w, 'b) With_attribute(Context)(Attribute).t) Map.Poly.t

  let declare ~name ~contexts ~pattern ~k =
    Map.Poly.of_iteri_exn ~iteri:(fun ~f ->
      List.iter contexts ~f:(fun (T context as key : _ Packed(Context).t) ->
        let attribute = Attribute.declare name (Context.to_ppxlib context) pattern k in
        f ~key ~data:(T (context, attribute) : _ With_attribute(Context)(Attribute).t)) [@nontail
                                                                                          ])
  ;;

  let find_exn (type a w b) (t : (w, b) t) (ctx : (a, w) Context.t) : (a, b) Attribute.t =
    let (T (ctx', attribute)) = Map.find_exn t (T ctx) in
    let T = Context.same_witness_exn ctx ctx' in
    attribute
  ;;
end

module Context = struct
  type ('a, 'w) t =
    | Expression : (expression, [> `expression ]) t
    | Module_expr : (module_expr, [> `module_expr ]) t
    | Core_type : (core_type, [> `core_type ]) t
    | Module_type : (module_type, [> `module_type ]) t
    | Value_binding : (value_binding, [> `value_binding ]) t
    | Value_description : (value_description, [> `value_description ]) t
    | Module_binding : (module_binding, [> `module_binding ]) t
    | Module_declaration : (module_declaration, [> `module_declaration ]) t
    | Type_declaration : (type_declaration, [> `type_declaration ]) t
    | Module_type_declaration : (module_type_declaration, [> `module_type_declaration ]) t
    | Include_infos :
        ((module_expr, module_type) Either.t include_infos, [> `include_infos ]) t

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

  let same_witness_exn (type a b w) (a : (a, w) t) (b : (b, w) t) : (a, b) Type_equal.t =
    match a, b with
    | Expression, Expression -> T
    | Module_expr, Module_expr -> T
    | Core_type, Core_type -> T
    | Module_type, Module_type -> T
    | Value_binding, Value_binding -> T
    | Value_description, Value_description -> T
    | Module_binding, Module_binding -> T
    | Module_declaration, Module_declaration -> T
    | Type_declaration, Type_declaration -> T
    | Module_type_declaration, Module_type_declaration -> T
    | Include_infos, Include_infos -> T
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

let consume t ctx ast = Attribute.consume (find_exn t ctx) ast
let get t ctx ast = Attribute.get (find_exn t ctx) ~mark_as_seen:false ast
let has t ctx ast = Option.is_some (get t ctx ast)

let declare_poly (type a cmp) (id : (a, cmp) Identifier.t) ~name =
  declare
    ~name
    ~contexts:
      [ T Value_binding
      ; T Value_description
      ; T Module_binding
      ; T Module_declaration
      ; T Type_declaration
      ; T Module_type_declaration
      ; T Include_infos
      ]
    ~pattern:(ident_poly_pattern (module (val id)))
    ~k:(Bindings.create id)
;;

let kind_poly = declare_poly Identifier.kind ~name:"kind"
let mode_poly = declare_poly Identifier.mode ~name:"mode"

let declare_mono (type a cmp) (id : (a, cmp) Identifier.t) ~name =
  declare
    ~name
    ~contexts:[ T Expression; T Module_expr; T Core_type; T Module_type ]
    ~pattern:(ident_mono_pattern (module (val id)))
    ~k:Fn.id
;;

let kind_mono = declare_mono Identifier.kind ~name:"kind"
let mode_mono = declare_mono Identifier.mode ~name:"mode"

let exclave_if_local =
  declare
    ~name:"exclave_if_local"
    ~contexts:[ T Expression ]
    ~pattern:(Ast_pattern.single_expr_payload (ident Identifier.mode))
    ~k:Fn.id
;;

module Floating = struct
  module Context = struct
    type ('a, 'w) t =
      | Structure_item : (structure_item, [> `structure_item ]) t
      | Signature_item : (signature_item, [> `signature_item ]) t

    type 'a poly = ('a, [ `structure_item | `signature_item ]) t

    let to_ppxlib : type a w. (a, w) t -> a Attribute.Floating.Context.t = function
      | Structure_item -> Structure_item
      | Signature_item -> Signature_item
    ;;

    let same_witness_exn (type a b w) (a : (a, w) t) (b : (b, w) t) : (a, b) Type_equal.t =
      match a, b with
      | Structure_item, Structure_item -> T
      | Signature_item, Signature_item -> T
      | Structure_item, Signature_item | Signature_item, Structure_item -> assert false
    ;;
  end

  include Make (Attribute.Floating) (Context)

  let convert t ctx ast = Attribute.Floating.convert [ find_exn t ctx ] ast

  let declare_poly (type a cmp) (id : (a, cmp) Identifier.t) ~name =
    declare
      ~name
      ~contexts:[ T Structure_item; T Signature_item ]
      ~pattern:(ident_poly_pattern (module (val id)))
      ~k:(Bindings.create id)
  ;;

  let kind_poly = declare_poly Identifier.kind ~name:"kind"
  let mode_poly = declare_poly Identifier.mode ~name:"mode"
end
