open! Base
open! Import

type poly :=
  [ `value_binding
  | `value_description
  | `module_binding
  | `module_declaration
  | `type_declaration
  | `module_type_declaration
  | `include_infos
  ]

type mono :=
  [ `expression
  | `module_expr
  | `core_type
  | `module_type
  ]

module Context : sig
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

  type nonrec 'a poly = ('a, poly) t
  type nonrec 'a mono = ('a, mono) t
end

type ('w, 'b) t

val kind_poly : (poly, Bindings.M(Identifier.Kind)(Binding.Kind).t Or_error.t) t
val mode_poly : (poly, Bindings.M(Identifier.Mode)(Binding.Mode).t Or_error.t) t
val kind_mono : (mono, Binding.Kind.t list) t
val mode_mono : (mono, Binding.Mode.t list) t
val exclave_if_local : ([ `expression ], Identifier.Mode.t) t
val consume : ('w, 'b) t -> ('a, 'w) Context.t -> 'a -> ('a * 'b) option
val get : ('w, 'b) t -> ('a, 'w) Context.t -> 'a -> 'b option
val has : ('w, _) t -> ('a, 'w) Context.t -> 'a -> bool

module Floating : sig
  type poly :=
    [ `structure_item
    | `signature_item
    ]

  module Context : sig
    type ('a, 'w) t =
      | Structure_item : (structure_item, [> `structure_item ]) t
      | Signature_item : (signature_item, [> `signature_item ]) t

    type nonrec 'a poly = ('a, poly) t
  end

  type ('w, 'b) t

  val kind_poly : (poly, Bindings.M(Identifier.Kind)(Binding.Kind).t Or_error.t) t
  val mode_poly : (poly, Bindings.M(Identifier.Mode)(Binding.Mode).t Or_error.t) t
  val convert : ('w, 'b) t -> ('a, 'w) Context.t -> 'a -> 'b option
end
