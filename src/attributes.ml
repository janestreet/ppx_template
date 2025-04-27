open! Stdppx
open! Import

let at_most_one_pattern p = Ast_pattern.(p ^:: nil ||| map0 nil ~f:[])
let at_most_one_eval p = Ast_pattern.(pstr (at_most_one_pattern (pstr_eval p nil)))

let ident (type a) ((module Id) : a Identifier.t) =
  Ast_pattern.(pexp_ident (map1 (lident __) ~f:Id.of_string))
;;

let one_or_many a b = Ast_pattern.(map1 a ~f:(fun x -> [ x ]) ||| b)
let tuple_or_one p = one_or_many p Ast_pattern.(pexp_tuple (many p))

type 'a pat = { pat : 'b. unit -> (expression, 'a -> 'b, 'b) Ast_pattern.t } [@@unboxed]

let one_or_many_as_list { pat } =
  let open Ast_pattern in
  one_or_many
    (pat ())
    (map2 (pexp_apply (pat ()) (many (pair nolabel (pat ())))) ~f:List.cons)
;;

(* Parses an [expression] of the form [a b c] (i.e. function application of bindings)
   as [[ "a"; "b"; "c" ]]. *)
let binding_apply_pattern (type a id node) ((module Binding) : (a, id, node) Binding.t) =
  one_or_many_as_list { pat = Binding.pattern }
;;

(* Parses an [expression] of the form [a, b, c] (i.e. a tuple of bindings) as
   [[ "a"; "b"; "c" ]]. *)
let binding_tuple_pattern (type a id node) ((module Binding) : (a, id, node) Binding.t) =
  tuple_or_one (Binding.pattern ())
;;

(* Parses an [expression] of the form [l = (a, b)] as ["l", [ "a"; "b" ]]. *)
let ident_equals_pattern id binding =
  let open Ast_pattern in
  pexp_apply
    (pexp_ident (lident (string "=")))
    (pair nolabel (ident id) ^:: pair nolabel (binding_tuple_pattern binding) ^:: nil)
  |> pack2
;;

(* Parses a [payload] of the form [l = (a, b), m = (c, d)] as
   [[ "l", [ "a"; "b" ]; "m", [ "c"; "d" ] ]], or of the form [a b] as
   [[ "a", [ "a" ]; "b", [ "b" ]]]. *)
let binding_poly_pattern
  (type id binding node)
  (id : id Identifier.t)
  ((module Binding) as binding : (binding, id, node) Binding.t)
  =
  let open Ast_pattern in
  tuple_or_one (ident_equals_pattern id binding)
  ||| map1
        (binding_apply_pattern binding)
        ~f:
          (List.map ~f:(fun binding -> Binding.to_mangled_identifier binding, [ binding ]))
  |> at_most_one_eval
;;

(* Parses a [payload] of the form [a b c] as [[ "a"; "b"; "c" ]]. *)
let binding_mono_pattern binding = binding_apply_pattern binding |> at_most_one_eval

module type Context = sig
  type ('a, 'w) t : immediate
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

module Map_poly : sig
  type ('key, 'data) t

  val find_exn : ('key, 'data) t -> 'key -> 'data
  val of_list : ('key * 'data) list -> ('key, 'data) t
end = struct
  module Key = struct
    type t = Poly : _ -> t

    let compare = Poly.compare
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
       val same_witness_exn : ('a, 'w) t -> ('b, 'w) t -> ('a, 'b) Type.eq
     end) =
struct
  let declare ~name ~contexts ~pattern ~k =
    Map_poly.of_list
      (List.map contexts ~f:(fun (T context as key : _ Packed(Context).t) ->
         let attribute =
           Attribute.declare ("template." ^ name) (Context.to_ppxlib context) pattern k
         in
         key, (T (context, attribute) : _ With_attribute(Context)(Attribute).t)))
  ;;

  let find_exn
    (type a w b)
    (t : (w Packed(Context).t, (w, b) With_attribute(Context)(Attribute).t) Map_poly.t)
    (ctx : (a, w) Context.t)
    : (a, b) Attribute.t
    =
    let (T (ctx', attribute)) = Map_poly.find_exn t (T ctx) in
    let Equal = Context.same_witness_exn ctx ctx' in
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

  type 'a zero_alloc_if_local =
    ('a, [ `expression | `value_binding | `value_description ]) t

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

  let same_witness_exn (type a b w) (a : (a, w) t) (b : (b, w) t) : (a, b) Type.eq =
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

let consume_attr attr ctx ast = Attribute.consume (find_exn attr ctx) ast

type ('w, 'b) t = { f : 'a. ('a, 'w) Context.t -> 'a -> 'a * 'b } [@@unboxed]

let consume t ctx item = t.f ctx item

module Poly = struct
  let contexts : _ Packed(Context).t list =
    [ T Value_binding
    ; T Value_description
    ; T Module_binding
    ; T Module_declaration
    ; T Type_declaration
    ; T Module_type_declaration
    ; T Include_infos
    ]
  ;;

  let declare
    (type id binding node)
    (id : id Identifier.t)
    (binding : (binding, id, node) Binding.t)
    ~name
    =
    declare
      ~name
      ~contexts
      ~pattern:(binding_poly_pattern id binding)
      ~k:(Bindings.create id binding)
  ;;

  let kind_attr = declare Identifier.kind Binding.kind ~name:"kind"
  let mode_attr = declare Identifier.mode Binding.mode ~name:"mode"
  let modality_attr = declare Identifier.modality Binding.modality ~name:"modality"

  type t =
    { kinds : Bindings.M(Identifier.Kind)(Binding.Kind).t option
    ; modes : Bindings.M(Identifier.Mode)(Binding.Mode).t option
    ; modalities : Bindings.M(Identifier.Modality)(Binding.Modality).t option
    }
end

let consume_poly ctx item =
  let consume attr item ~k =
    match consume_attr attr ctx item with
    | None -> k item None
    | Some (item, Ok kind) -> k item (Some kind)
    | Some (item, (Error _ as err)) -> item, err
  in
  consume Poly.kind_attr item ~k:(fun item kinds ->
    consume Poly.mode_attr item ~k:(fun item modes ->
      consume Poly.modality_attr item ~k:(fun item modalities ->
        item, Ok ({ kinds; modes; modalities } : Poly.t))))
;;

let poly = { f = consume_poly }

module Mono = struct
  let contexts : _ Packed(Context).t list =
    [ T Expression; T Module_expr; T Core_type; T Module_type ]
  ;;

  let declare binding ~name =
    declare ~name ~contexts ~pattern:(binding_mono_pattern binding) ~k:Fn.id
  ;;

  let kind_attr = declare Binding.kind ~name:"kind"
  let mode_attr = declare Binding.mode ~name:"mode"
  let modality_attr = declare Binding.modality ~name:"modality"

  type t =
    { kinds : Binding.Kind.t list
    ; modes : Binding.Mode.t list
    ; modalities : Binding.Modality.t list
    }
end

let consume_mono ctx item =
  let consume attr item =
    match consume_attr attr ctx item with
    | None -> item, []
    | Some (item, vals) -> item, vals
  in
  let item, kinds = consume Mono.kind_attr item in
  let item, modes = consume Mono.mode_attr item in
  let item, modalities = consume Mono.modality_attr item in
  item, ({ kinds; modes; modalities } : Mono.t)
;;

let mono = { f = consume_mono }

module Exclave_if_local = struct
  let attr =
    declare
      ~name:"exclave_if_local"
      ~contexts:[ T Expression ]
      ~pattern:(Ast_pattern.single_expr_payload (ident Identifier.mode))
      ~k:Fn.id
  ;;
end

let consume_attr_if_local attr ctx item =
  match consume_attr attr ctx item with
  | None -> item, None
  | Some (item, mode) -> item, Some mode
;;

let exclave_if_local =
  { f = (fun ctx item -> consume_attr_if_local Exclave_if_local.attr ctx item) }
;;

module Zero_alloc_if_local = struct
  let pattern =
    let open Ast_pattern in
    single_expr_payload
      (map (ident Identifier.mode) ~f:(fun k mode -> k mode [])
       ||| pexp_apply (ident Identifier.mode) (many (pair nolabel __)))
    |> map2' ~f:(fun loc mode payload -> loc, mode, payload)
  ;;

  let attr =
    declare
      ~name:"zero_alloc_if_local"
      ~contexts:[ T Expression; T Value_binding; T Value_description ]
      ~pattern
      ~k:Fn.id
  ;;
end

let zero_alloc_if_local =
  { f = (fun ctx item -> consume_attr_if_local Zero_alloc_if_local.attr ctx item) }
;;

module Conflate = struct
  let attr contexts identifier name =
    let pat () =
      ident identifier |> Ast_pattern.map1' ~f:(fun loc x -> Loc.make ~loc x)
    in
    declare
      ~name:("conflate_" ^ name)
      ~contexts
      ~pattern:(one_or_many_as_list { pat } |> Ast_pattern.single_expr_payload)
      ~k:Fn.id
  ;;

  let mono_modes = attr Mono.contexts Identifier.mode "mode_as_modality"
  let mono_modalities = attr Mono.contexts Identifier.modality "modality_as_mode"
  let poly_modes = attr Poly.contexts Identifier.mode "mode_as_modality"
  let poly_modalities = attr Poly.contexts Identifier.modality "modality_as_mode"
end

let make_conflate attr =
  let consume ctx item =
    match consume_attr attr ctx item with
    | None -> item, []
    | Some (item, modes) -> item, modes
  in
  { f = consume }
;;

let conflate_mono_modes = make_conflate Conflate.mono_modes
let conflate_mono_modalities = make_conflate Conflate.mono_modalities
let conflate_poly_modes = make_conflate Conflate.poly_modes
let conflate_poly_modalities = make_conflate Conflate.poly_modalities

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

    let same_witness_exn (type a b w) (a : (a, w) t) (b : (b, w) t) : (a, b) Type.eq =
      match a, b with
      | Structure_item, Structure_item -> Equal
      | Signature_item, Signature_item -> Equal
      | Structure_item, Signature_item | Signature_item, Structure_item -> assert false
    ;;
  end

  include Make (Attribute.Floating) (Context)

  let convert_attrs attrs ctx ast =
    Attribute.Floating.convert (List.map attrs ~f:(fun attr -> find_exn attr ctx)) ast
  ;;

  module Poly = struct
    module Bindings = struct
      type t =
        | Kinds of Bindings.M(Identifier.Kind)(Binding.Kind).t
        | Modes of Bindings.M(Identifier.Mode)(Binding.Mode).t
        | Modalities of Bindings.M(Identifier.Modality)(Binding.Modality).t

      let kinds b = Kinds b
      let modes b = Modes b
      let modalities b = Modalities b
    end

    type t =
      { bindings : Bindings.t
      ; default : bool
      }
  end

  let declare_poly
    (type id binding node)
    (poly : (id, binding) Bindings.t -> Poly.Bindings.t)
    (id : id Identifier.t)
    (binding : (binding, id, node) Binding.t)
    ~name
    =
    List.map [ false; true ] ~f:(fun default ->
      let name = if default then name ^ ".default" else name in
      declare
        ~name
        ~contexts:[ T Structure_item; T Signature_item ]
        ~pattern:(binding_poly_pattern id binding)
        ~k:(fun binds ->
          Result.map (Bindings.create id binding binds) ~f:(fun bindings : Poly.t ->
            { bindings = poly bindings; default })))
  ;;

  let kind_poly =
    declare_poly Poly.Bindings.kinds Identifier.kind Binding.kind ~name:"kind"
  ;;

  let mode_poly =
    declare_poly Poly.Bindings.modes Identifier.mode Binding.mode ~name:"mode"
  ;;

  let modality_poly =
    declare_poly
      Poly.Bindings.modalities
      Identifier.modality
      Binding.modality
      ~name:"modality"
  ;;

  let convert_poly ctx ast = convert_attrs (kind_poly @ mode_poly @ modality_poly) ctx ast
end
