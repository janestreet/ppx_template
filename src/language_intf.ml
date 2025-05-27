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

    Grammar of poly-attribute payloads:
    {v
    # e.g. let f x = x [@@kind k1 = bits32, k2 = (value, bits32)] [@@mode local local]
    poly<t> ::= simple-bindings<t> | punned-bindings<t>

    simple-bindings<t> ::= nil | simple-binding<t> ("," simple-binding<t>)*

    simple-binding<t> ::= pattern<t> "=" expressions<t>

    punned-bindings<t> ::= expression<t>*
    v}

    Grammar of mono-attribute payloads:
    {v
    # e.g. let x = (f [@kind value bits32])
    mono<t> ::= expression<t>*
    v}

    Patterns:
    {v
    pattern<mode | modality | kind | alloc> ::= identifier

    pattern<alloc_at_mode> ::= pattern<alloc> "@" pattern<mode>
    v}

    Expressions:
    {v
    expressions<t> ::= expression<t> | "(" expression<t> ("," expression<t>)* ")"

    expression<mode> ::= identifier

    expression<modality> ::= identifier

    expression<kind> ::=
    | identifier
    | expression<kind> ("&" expression<kind>)+
    | expression<kind> "mod" expression<mode>+

    expression<alloc> ::= identifier

    expression<alloc_at_mode> ::=
    | identifier
    | identifier "@" identifier
    v} *)

module Definitions = struct
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
      | Tuple2 : 'a t * 'b t -> ('a * 'b) t

    type packed = P : 'a t -> packed [@@unboxed]

    module Basic = struct
      type packed = P : 'a basic t -> packed [@@unboxed]
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
      | Tuple2 : 'a t * 'b t -> ('a * 'b) t

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
      | Tuple2 : 'a t * 'b t -> ('a * 'b) t

    type packed = P : 'a t -> packed [@@unboxed]

    module Basic = struct
      type packed = P : 'a Type.basic t -> packed [@@unboxed]
    end
  end

  module Pattern = struct
    type 'a t =
      | Identifier : 'a Identifier.t -> 'a t
      | Tuple2 : 'a t * 'b t -> ('a * 'b) t
  end

  module Env = struct
    (** An association list mapping identifiers to values. Entries are added to the front
        of the association list, and looked up from front to back, so newer entries shadow
        older ones. *)

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
      (** When an item [let lhs = rhs [@@attr pat = (expr1, expr2)]] is evaluated e.g. on
          [expr1], the expression gets evaluated to a [Value.t], which is then passed to
          [mangle] to create a [Mangler.t] that is used to mangle [lhs]. This is used to
          enable [[@@alloc (a @ m) = ...]] to mangle only based on [a]. *)
      }
      constraint 'mangle = _ Type.basic

    type packed = P : _ t -> packed
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

module type Language = sig
  include module type of struct
    include Definitions
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

    module Map : Map.S with type key := packed

    val sexp_of_t : _ t -> Sexp.t
    val kind : kind t
    val mode : mode t
    val modality : modality t
    val alloc : alloc t
    val tuple2 : 'a t -> 'b t -> ('a * 'b) t
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
    val sexp_of_t : _ t -> Sexp.t
    val type_ : 'a t -> 'a Type.t

    (** Convert a value in the template language to a concrete OCaml AST node. *)
    val to_node : 'a Type.basic t -> loc:location -> 'a Type.basic Node.t
  end

  module Pattern : sig
    include module type of struct
      include Pattern
    end

    val compare : 'a t -> 'a t -> int
    val sexp_of_t : _ t -> Sexp.t
  end

  module Expression : sig
    include module type of struct
      include Expression
    end

    val compare : 'a t -> 'a t -> int
    val sexp_of_t : _ t -> Sexp.t
    val type_ : 'a t -> 'a Type.t
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

    (** Evaluates an expression in the given environment. Unbound [Expression.Identifier]s
        are evaluated as an equivalent [Value.Identifier] under the assumption that the
        identifier will be interpreted by the OCaml compiler; if it is not, we let the
        compiler report the error to the user. *)
    val eval : t -> 'a Expression.t Loc.t -> 'a Value.t
  end
end
