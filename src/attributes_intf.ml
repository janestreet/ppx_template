open! Stdppx
open! Import
open Language

module Definitions = struct
  module Poly = struct
    type t = Poly : 'mangle Type.t * ('a, 'mangle) Binding.t list -> t
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
      | Module_type_declaration :
          (module_type_declaration, [> `module_type_declaration ]) t
      | Include_infos :
          ((module_expr, module_type) Either.t include_infos, [> `include_infos ]) t
  end
end

module type Attributes = sig
  include module type of struct
    include Definitions
  end

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

  type zero_alloc_if :=
    [ `expression
    | `value_binding
    | `value_description
    ]

  module Context : sig
    include module type of struct
      include Context
    end

    type 'w packed = private T : (_, 'w) t -> 'w packed [@@unboxed]
    type nonrec 'a poly = ('a, poly) t
    type nonrec 'a mono = ('a, mono) t
    type nonrec 'a zero_alloc_if = ('a, zero_alloc_if) t
  end

  (** A [('w, 'b) t] is a handler that knows how to consume a particular attribute on ['w]
      syntax items, and produce back ['b] values. Note: if the attribute isn't present,
      the handler will return some default ['b], whether that be [None] (if ['b] is
      [_ option]) or some semantic default for the given ['b]. *)
  type ('w, 'b) t

  (** [consume t ctx item] runs the handler [t] in the context [ctx] on [item]. The
      handler [t] strips its corresponding attributes from [item] in addition to producing
      its output. *)
  val consume : ('w, 'b) t -> ('a, 'w) Context.t -> 'a -> 'a * 'b

  (** A map from [('a, 'w) Context.t] to [('a, 'b) Attribute.t]. *)
  module Attribute_map : sig
    type ('w, 'b) t

    val find_exn : ('w, 'b) t -> ('a, 'w) Context.t -> ('a, 'b) Attribute.t
  end

  module Poly : module type of struct
    include Poly
  end

  (** A handler for attributes that make definitions/declarations polymorphic. Might
      return an [Error _] if the attribute's payload is malformed. Defaults to
      [Ok { kinds = None; modes = None }]. *)
  val poly : (poly, (Poly.t list, Sexp.t) result) t

  module Mono : sig
    type t = Expression.Basic.packed Loc.t list Type.Map.t

    val contexts : mono Context.packed list
    val kind_attr : (mono, Expression.Basic.packed Loc.t list) Attribute_map.t
    val mode_attr : (mono, Expression.Basic.packed Loc.t list) Attribute_map.t
    val modality_attr : (mono, Expression.Basic.packed Loc.t list) Attribute_map.t
    val alloc_attr : (mono, Expression.Basic.packed Loc.t list) Attribute_map.t
  end

  (** A handler for attributes that mangle identifiers to the correct monomorphized name.
      Defaults to [{ kinds = []; modes = [] }]. *)
  val mono : (mono, Mono.t) t

  (** A handler for attributes that optionally insert [exclave_] markers. We expect to
      replace these attributes with mode-polymorphic tailcalls and/or unboxed types. *)
  val exclave_if_local : ([ `expression ], Type.mode Expression.t Loc.t option) t

  (** Like {!exclave_if_local}, but for allocation identifiers. *)
  val exclave_if_stack : ([ `expression ], Type.alloc Expression.t Loc.t option) t

  (** A handler for attributes that optionally annotate code as zero-alloc. When the
      attribute is present, produces [Some (loc, mode, payload)], where [loc] is the
      location of the payload, [mode] is the mode to compare against, and [payload] is the
      payload to be given to the [[@@zero_alloc]] attribute. *)
  val zero_alloc_if_local
    : ( zero_alloc_if
        , (location * Type.mode Expression.t Loc.t * expression list) option )
        t

  (** Like {!zero_alloc_if_local}, but for allocation identifiers. *)
  val zero_alloc_if_stack
    : ( zero_alloc_if
        , (location * Type.alloc Expression.t Loc.t * expression list) option )
        t

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

    module Poly : sig
      type t =
        { bindings : Poly.t
        ; default : bool
        }
    end

    val convert_poly : ('a, poly) Context.t -> 'a -> (Poly.t, Sexp.t) result option
  end
end
