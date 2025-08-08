open! Stdppx
open! Import

(** This module defines the underlying DSL of [ppx_template]. Below, we provide an
    overview of the semantics of the DSL, as well as a grammar for how the surface syntax
    corresponds to the DSL.

    While inside of a [[%template]] node, an environment ([Env.t]) is accumulated that
    maps identifiers ([Identifier.t]) to values ([Value.t]). Each identifier and value is
    associated with a type ([Type.t]) and identifiers are namespaced by their type.

    The environment is updated by poly-attributes. A poly-attribute is either floating (in
    which case it updates the environment for all subsequent items in the current
    structure/signature) or is attached to an item (in which case it updates the
    environment for the item it is directly attached to).

    A poly-attribute consists of a list of bindings; if there are multiple
    poly-attributes, the lists of bindings can conceptually be concatenated to form a
    single list of bindings. Bindings are evaluated sequentially. An individual binding
    maps a pattern ([Pattern.t]) to a list of expressions ([Expression.t]). Evaluation is
    split into a branch for each expression. The expression is evaluated under the current
    environment, and is combined with the pattern to form new (identifier x expression)
    entries which are added to the environment for the branch.

    The environment is used to update the OCaml AST in two ways:

    1) Each type is associated with a node in the OCaml AST (via [Node.t]), and when such
       a node is reached within a [[%template]] body, any OCaml identifiers within that
       node will be looked up in the current environment and, if found, replaced with
       their associated value.

    2) If a mono-attribute is encountered (a [[@kind]], [[@mode]], [[@modality]], or
       [[@alloc]] attribute attached to an identifier), its payload (a list of
       expressions) is evaluated in the current environment, and is used to mangle the
       attached identifier. If there are multiple mono-attributes, the lists of
       expressions are concatenated.

    Below, we define a grammar for the payloads of [ppx_template] attributes and how they
    correspond to the types in the [ppx_template] DSL.

    Helper grammars:
    {v
    tuple<x> ::=
    | "(" x ("," x)+ ")"
    | "(" x "@" x ")"

    identifier ::= (valid ocaml variable identifier)
    v}

    Grammar of poly-attribute payloads:
    {v
    # e.g. let f x = x [@@kind k1 = bits32, k2 = (value, bits32)] [@@mode local local]
    poly ::= simple-bindings | punned-bindings

    simple-bindings ::= nil | simple-binding ("," simple-binding)*

    simple-binding ::= pattern "=" expressions

    punned-bindings ::= expression*
    v}

    Grammar of mono-attribute payloads:
    {v
    # e.g. let x = (f [@kind value bits32])
    mono ::= expression*
    v}

    Patterns:
    {v
    pattern ::=
    | identifier
    | tuple<identifier>
    v}

    Expressions:
    {v
    expressions ::= expression | "(" expression ("," expression)+ ")"

    expression ::=
    | identifier
    | expression ("&" expression)+
    | expression "mod" expression+
    | tuple<expression>
    v} *)

module Definitions = struct
  module Untyped = struct
    module Axis = struct
      type t =
        | Kind
        | Mode
        | Modality
        | Alloc
    end

    module Identifier = struct
      type t = { ident : string }
    end

    module Expression = struct
      type t =
        | Identifier of Identifier.t
        | Kind_product of t list
        | Kind_mod of t * t list
        | Tuple of t list
    end

    module Value = struct
      type t =
        | Identifier of Identifier.t
        | Kind_product of t list
        | Kind_mod of t * t list
        | Tuple of t list
    end

    module Pattern = struct
      type t =
        | Identifier of Identifier.t
        | Tuple of t list
    end
  end

  module Typed = struct
    module Type = struct
      (*_ Note: we only provide concrete definitions for these types so that the compiler
        knows they are distinct. They are [private] so that they can only ever be used
        as phantom types. *)
      type kind_ = private Kind
      type mode_ = private Mode
      type modality_ = private Modality
      type alloc_ = private Alloc

      type _ basic =
        | Kind : kind_ basic
        | Mode : mode_ basic
        | Modality : modality_ basic
        | Alloc : alloc_ basic

      type kind = kind_ basic
      type mode = mode_ basic
      type modality = modality_ basic
      type alloc = alloc_ basic

      type _ t =
        | Basic : 'a basic -> 'a basic t
        | Tuple : 'a tuple -> 'a t

      and _ tuple =
        | [] : unit tuple
        | ( :: ) : 'a t * 'b tuple -> ('a * 'b) tuple

      type packed = P : 'a t -> packed [@@unboxed]

      module Basic = struct
        type packed = P : 'a basic t -> packed [@@unboxed]
      end
    end

    module Axis = struct
      type _ t =
        | Kind : Type.kind_ Type.basic t
        | Mode : Type.mode_ Type.basic t
        | Modality : Type.modality_ Type.basic t
        | Alloc : Type.alloc_ Type.basic t

      type packed = P : _ t -> packed

      module Sub_axis = struct
        module Modal = struct
          type t =
            | Locality
            | Portability
            | Contention
            | Affinity
            | Uniqueness
            | Yielding
        end

        module Mode = struct
          type t = Mode of Modal.t [@@unboxed]
        end

        module Modality = struct
          type t = Modality of Modal.t [@@unboxed]
        end

        module Or_unrecognized = struct
          type 'a t =
            | Known of 'a
            | Unrecognized
        end

        type _ t =
          | Kind : Type.kind_ Type.basic t
          | Mode : Mode.t Or_unrecognized.t -> Type.mode_ Type.basic t
          | Modality : Modality.t Or_unrecognized.t -> Type.modality_ Type.basic t
          | Alloc : Type.alloc_ Type.basic t

        type packed = P : _ t -> packed
      end
    end

    module Identifier = struct
      type 'a t =
        { type_ : 'a Type.t
        ; ident : string
        }
    end

    module Expression = struct
      type 'a t =
        | Identifier : 'a Identifier.t -> 'a t
        | Kind_product : Type.kind t list -> Type.kind t
        | Kind_mod : Type.kind t * Type.modality t list -> Type.kind t
        | Tuple : 'a tuple -> 'a t

      and _ tuple =
        | [] : unit tuple
        | ( :: ) : 'a t * 'b tuple -> ('a * 'b) tuple

      type packed = P : 'a t -> packed [@@unboxed]

      module Basic = struct
        type packed = P : 'a Type.basic t -> packed [@@unboxed]
      end
    end

    module Value = struct
      type 'a t =
        | Identifier : 'a Type.basic Identifier.t -> 'a Type.basic t
        | Kind_product : Type.kind t list -> Type.kind t
        | Kind_mod : Type.kind t * Type.modality t list -> Type.kind t
        | Tuple : 'a tuple -> 'a t

      and _ tuple =
        | [] : unit tuple
        | ( :: ) : 'a t * 'b tuple -> ('a * 'b) tuple

      type packed = P : 'a t -> packed [@@unboxed]

      module Basic = struct
        type packed = P : 'a Type.basic t -> packed [@@unboxed]
      end
    end

    module Pattern = struct
      type 'a t =
        | Identifier : 'a Identifier.t -> 'a t
        | Tuple : 'a tuple -> 'a t

      and _ tuple =
        | [] : unit tuple
        | ( :: ) : 'a t * 'b tuple -> ('a * 'b) tuple
    end

    module Env = struct
      (** An association list mapping identifiers to values. Entries are added to the
          front of the association list, and looked up from front to back, so newer
          entries shadow older ones. *)

      module Entry = struct
        type t = Entry : 'a Identifier.t * 'a Value.t -> t
      end

      type t = Entry.t list
    end

    module Binding = struct
      type ('a, 'mangle) t =
        { pattern : 'a Pattern.t
        ; expressions : 'a Expression.t Loc.t list
        ; mangle : 'a Value.t -> 'mangle Value.t
        (** When an item [let lhs = rhs [@@attr pat = (expr1, expr2)]] is evaluated e.g.
            on [expr1], the expression gets evaluated to a [Value.t], which is then passed
            to [mangle] to create a [Mangler.t] that is used to mangle [lhs]. This is used
            to enable [[@@alloc (a @ m) = ...]] to mangle only based on [a]. *)
        ; mangle_axis : 'mangle Axis.t
        }
        constraint 'mangle = _ Type.basic
    end

    module Node = struct
      (** A type used as the output of [Value.to_node]. *)
      type 'a t =
        | Jkind_annotation : jkind_annotation -> Type.kind t
        | Mode : mode loc -> Type.mode t
        | Modality : modality loc -> Type.modality t
        | Alloc : Type.alloc t
    end
  end

  module Type_error = struct
    type t =
      | Type_mismatch :
          { kind : string
          ; sexp_of_kind : 'value -> Sexp.t
          ; value : 'value
          ; expected_type : _ Typed.Type.t
          }
          -> t
      | Tuple_length_mismatch :
          { kind : string
          ; sexp_of_kind : 'value -> Sexp.t
          ; value : 'value
          ; expected_type : _ Typed.Type.t
          }
          -> t
  end
end

module type Language = sig
  include module type of struct
    include Definitions
  end

  module Untyped : sig
    include module type of struct
      include Untyped
    end

    module Axis : sig
      include module type of struct
        include Axis
      end

      module Map : Map.S with type key = t
    end

    module Identifier : sig
      include module type of struct
        include Identifier
      end
    end

    module Expression : sig
      include module type of struct
        include Expression
      end

      val compare : t -> t -> int
      val sexp_of_t : t -> Sexp.t
    end

    module Value : sig
      include module type of struct
        include Value
      end

      val compare : t -> t -> int
      val sexp_of_t : t -> Sexp.t
    end

    module Pattern : sig
      include module type of struct
        include Pattern
      end

      val compare : t -> t -> int
      val sexp_of_t : t -> Sexp.t
    end
  end

  module Typed : sig
    include module type of struct
      include Typed
    end

    module Type : sig
      include module type of struct
        include Type
      end

      module Basic : sig
        include module type of struct
          include Basic
        end
      end

      val sexp_of_t : _ t -> Sexp.t
      val kind : kind t
      val mode : mode t
      val modality : modality t
      val alloc : alloc t
      val tuple2 : 'a t -> 'b t -> ('a * ('b * unit)) t
    end

    module Axis : sig
      include module type of struct
        include Axis
      end

      module Map : Map.S with type key := packed

      val of_type : 'a Type.basic Type.t -> 'a Type.basic t

      module Sub_axis : sig
        include module type of struct
          include Sub_axis
        end

        module Modal : sig
          include module type of struct
            include Modal
          end
        end

        module Mode : sig
          include module type of struct
            include Mode
          end
        end

        module Modality : sig
          include module type of struct
            include Modality
          end
        end

        module Map : Stdlib.Map.S with type key := packed

        val of_identifier : 'a Type.basic Identifier.t -> 'a Type.basic t
        val of_value : 'a Type.basic Value.t -> 'a Type.basic t
      end
    end

    module Identifier : sig
      include module type of struct
        include Identifier
      end
    end

    module Value : sig
      include module type of struct
        include Value
      end

      val compare : 'a t -> 'a t -> int

      (** Checks whether a value is a default value for its axis. *)
      val is_default : 'a Type.basic t -> bool

      (** Convert a value in the template language to a concrete OCaml AST node. *)
      val to_node : 'a Type.basic t -> loc:location -> 'a Type.basic Node.t
    end

    module Pattern : sig
      include module type of struct
        include Pattern
      end

      val untype : 'a t -> Untyped.Pattern.t

      val type_check
        :  Untyped.Pattern.t
        -> expected:'a Type.t
        -> ('a t, Type_error.t) result
    end

    module Expression : sig
      include module type of struct
        include Expression
      end

      val untype : 'a t -> Untyped.Expression.t

      val type_check
        :  Untyped.Expression.t
        -> expected:'a Type.t
        -> ('a t, Type_error.t) result
    end

    module Env : sig
      include module type of struct
        include Env
      end

      (** An [Env.t] populated with initial bindings for [heap] and [stack] *)
      val initial : t

      val find : t -> 'a Identifier.t -> 'a Value.t option

      (** Adds a new binding to the environment. *)
      val bind : t -> 'a Pattern.t -> 'a Value.t -> t

      (** Evaluates an expression in the given environment. Unbound
          [Expression.Identifier]s are evaluated as an equivalent [Value.Identifier] under
          the assumption that the identifier will be interpreted by the OCaml compiler; if
          it is not, we let the compiler report the error to the user. *)
      val eval : t -> 'a Expression.t Loc.t -> 'a Value.t
    end
  end

  module Type_error : sig
    include module type of struct
      include Type_error
    end

    val sexp_of_t : t -> Sexp.t
    val lift_to_error_result : ('a, t) result -> ('a, Sexp.t) result
  end
end
