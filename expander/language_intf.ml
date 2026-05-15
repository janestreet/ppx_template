open! Stdppx
open! Import

(** This module defines the underlying DSL of [ppx_template]. Below, we provide an
    overview of the semantics of the DSL, as well as a grammar for how the surface syntax
    corresponds to the DSL.

    While inside of a [[%template]] node, an environment ([Env.t]) is accumulated that
    maps identifiers ([Identifier.t]) to values ([Value.t]). Each identifier and value is
    associated with a type ([Type.t]) and identifiers are namespaced by their type.

    The environment is updated by two types of attributes:

    1. poly-attributes; a poly-attribute is either:
       a. floating (in which case it updates the environment for all subsequent items in
          the current structure/signature), e.g. [[@@@kind k = bits64]] or
       b. attached to an item (in which case it updates the environment for the item it is
          directly attached to), e.g. [[@@kind k = bits64]]

    2. define-attributes; a define attribute is always floating attribute, and is used to
       bind a set in the namespace [[@@@kind_set.define ks = (value, bits64)]]

    Poly-attributes and define-attributes both consist of a list of bindings; if there are
    multiple such attributes, the lists of bindings can conceptually be concatenated to
    form a single list of bindings. Bindings are evaluated sequentially. An individual
    binding maps a pattern ([Pattern.t]) to a single expression ([Expression.t]). For a
    pattern of type ['a], the associated expression evaluates to a set of values
    ([Value.t]) of type ['a].
    - For a poly-attribute, evaluation is split into a branch for each value in the set.
      The value is combined with the pattern to form new (identifier x expression) entries
      which are added to the environment for the branch.
    - For a define-attribute, no branching occurs; rather, the whole set is bound to the
      name in the environment.

    Note the distinction between:
    - [[@@@kind_set ks = (value, bits64)]], which is a poly-attribute that causes the
      following code to be templated with the two singleton sets [ks = value] and
      [ks = bits64] and
    - [[@@@kind_set.define ks = (value, bits64)]], which is a define-attribute and binds
      the name [ks] to the set [value, bits64] once.

    The environment is used to update the OCaml AST in two ways:

    1) Each type is associated with a node in the OCaml AST (via [Node.t]), and when such
       a node is reached within a [[%template]] body, any OCaml identifiers within that
       node will be looked up in the current environment and, if found, replaced with
       their associated value.

    2) If a mono-attribute is encountered (a [[@kind]], [[@mode]], [[@modality]], or
       [[@alloc]] attribute attached to an identifier), its payload (a list of
       expressions) is evaluated in the current environment, and is used to mangle the
       attached identifier. If there are multiple mono-attributes, the lists of
       expressions are concatenated. *)

module Definitions = struct
  module Type = struct
    (*_ Note: we only provide concrete definitions for these types so that the compiler
        knows they are distinct. They are [private] so that they can only ever be used as
        phantom types. *)
    type kind__ = private Kind
    type mode_ = private Mode
    type modality_ = private Modality
    type alloc_ = private Alloc
    type synchro_ = private Synchro

    type _ non_tuple =
      | Kind : kind__ non_tuple
      | Mode : mode_ non_tuple
      | Modality : modality_ non_tuple
      | Alloc : alloc_ non_tuple
      | Synchro : synchro_ non_tuple

    type kind = kind__ non_tuple
    type mode = mode_ non_tuple
    type modality = modality_ non_tuple
    type alloc = alloc_ non_tuple
    type synchro = synchro_ non_tuple

    type _ t =
      | Non_tuple : 'a non_tuple -> 'a non_tuple t
      | Tuple : ('a * 'b) tuple -> ('a * 'b) t (*_ Tuples have at least one element *)

    and _ tuple =
      | [] : unit tuple
      | ( :: ) : 'a t * 'b tuple -> ('a * 'b) tuple

    type packed = P : 'a t -> packed [@@unboxed]

    module Non_tuple = struct
      type packed = P : 'a non_tuple t -> packed [@@unboxed]
    end
  end

  module Untyped = struct
    module Axis = struct
      type t =
        | Kind
        | Mode
        | Modality
        | Alloc
        | Synchro
        | Set of t
    end

    module Identifier = struct
      type t = { ident : string }
    end

    module Expression = struct
      type t =
        | Identifier of Identifier.t
        | Kind_product of t Nonempty_list.t
        | Kind_mod of t * t Nonempty_list.t
        | Kind_coercion of t * t
        | Comma_separated of t Nonempty_list.t
        | Typed of t * Type.packed
    end

    module Value = struct
      type t =
        | Identifier of Identifier.t
        | Kind_product of t Nonempty_list.t
        | Kind_mod of t * t Nonempty_list.t
        | Tuple of t Nonempty_list.t
    end

    module Pattern = struct
      type t =
        | Wildcard
        | Identifier of Identifier.t
        | Tuple of t Nonempty_list.t
    end
  end

  module Typed = struct
    module Axis = struct
      type _ singleton =
        | Kind : Type.kind singleton
        | Mode : Type.mode singleton
        | Modality : Type.modality singleton
        | Alloc : Type.alloc singleton
        | Synchro : Type.synchro singleton

      type _ t =
        | Singleton : 'a singleton -> 'a t
        | Set : 'a singleton -> 'a t

      type packed = P : _ t -> packed [@@unboxed]

      module Sub_axis = struct
        module Modal = struct
          type t =
            | Locality
            | Portability
            | Contention
            | Statefulness
            | Visibility
            | Linearity
            | Uniqueness
            | Yielding
            | Forkable
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
          | Kind : Type.kind t
          | Mode : Mode.t Or_unrecognized.t -> Type.mode t
          | Modality : Modality.t Or_unrecognized.t -> Type.modality t
          | Alloc : Type.alloc t
          | Synchro : Type.synchro t

        type packed = P : _ t -> packed [@@unboxed]
      end

      module Namespace = struct
        type _ t =
          | Singleton : 'a Sub_axis.t -> 'a t
          | Set : 'a Sub_axis.t -> 'a t
          | Tuple : ('a * 'b) tuple -> ('a * 'b) t (*_ Tuples have at least one element *)

        and _ tuple =
          | [] : unit tuple
          | ( :: ) : 'a t * 'b tuple -> ('a * 'b) tuple
      end
    end

    module Identifier = struct
      type 'a t =
        { type_ : 'a Type.t
        ; ident : string
        }
    end

    module Expression = struct
      (** Expressions that recursively do not contain any syntactic unions, e.g.
          [k mod m], [base_with_imm], and [heap @ global]. These expressions can still be
          interpreted as sets when identifiers bound to sets are expanded. *)
      type singleton = private Singleton

      (** Expressions that might contain a syntactic union, such as any singleton, and
          also expressions like [(k1, k2) & k3]. *)
      type set = private Set

      type ('err, 'which) allow_set =
        | Singleton_only : { why_no_set : 'err } -> ('err, singleton) allow_set
        (** Error hint for expressions that contain illegal unions *)
        | Set_or_singleton : (_, set) allow_set

      (** ['a] is the type of [Value]s to which an [Expression] evaluates. ['s] is whether
          the expression is allowed to syntactically contain sets (corresponding to the
          [Union] constructor). If an [('a, singleton) t] is evaluated without expanding
          identifiers bound to sets, the result is just one ['a Value.t]. If a
          [('a, set) t] is evaluated, or when any [('a, _) t] is evaluated by expanding
          identifiers bound to sets, the result of evaluation is a list of ['a Value.t]s. *)
      type ('a, 's) t =
        | Identifier : 'a Identifier.t -> ('a, _) t
        | Kind_product : (Type.kind, 's) t Nonempty_list.t -> (Type.kind, 's) t
        | Kind_mod :
            (Type.kind, 's) t * (Type.modality, singleton) t Nonempty_list.t
            -> (Type.kind, 's) t
        | Kind_coercion : (Type.kind, 's) t * (Type.kind, set) t -> (Type.kind, 's) t
        | Tuple :
            ('a * 'b) tuple
            -> ('a * 'b, _) t (*_ Tuples have at least one element *)
        | Union : ('a, _) t Nonempty_list.t -> ('a, set) t

      and 'a tuple =
        | [] : unit tuple
        | ( :: ) : ('a, singleton) t * 'b tuple -> ('a * 'b) tuple

      type packed = P : ('a, 's) t -> packed [@@unboxed]

      module Basic = struct
        type packed = P : ('a Type.non_tuple, singleton) t -> packed [@@unboxed]
      end
    end

    module Value = struct
      type 'a t =
        | Identifier : 'a Type.non_tuple Identifier.t -> 'a Type.non_tuple t
        | Kind_product : Type.kind t Nonempty_list.t -> Type.kind t
        | Kind_mod : Type.kind t * Type.modality t Nonempty_list.t -> Type.kind t
        | Tuple : ('a * 'b) tuple -> ('a * 'b) t (*_ Tuples have at least one element *)

      and _ tuple =
        | [] : unit tuple
        | ( :: ) : 'a t * 'b tuple -> ('a * 'b) tuple

      type packed = P : 'a t -> packed [@@unboxed]

      module Basic = struct
        type packed = P : 'a Type.non_tuple t -> packed [@@unboxed]
      end

      module Defaultness = struct
        type t =
          | Actual_default (** Actual defaults in the mode/kind system. *)
          | Default_for_standard_mangling
          (** The special case of [[@kind value_or_null]] is mangled as default for
              pragmatic reasons. [[@kind.explicit_plus_unmangled value_or_null]] is not
              treated as default. *)
          | Not_a_default (** Values that are not a default for any purpose. *)
      end
    end

    module Pattern = struct
      type 'a t =
        | Wildcard : 'a t
        | Identifier : 'a Identifier.t -> 'a t
        | Tuple : ('a * 'b) tuple -> ('a * 'b) t (*_ Tuples have at least one element *)

      and _ tuple =
        | [] : unit tuple
        | ( :: ) : 'a t * 'b tuple -> ('a * 'b) tuple
    end
  end

  module Node = struct
    (** A type used as the output of [Value.to_node]. *)
    type 'a t =
      | Jkind_annotation : jkind_annotation -> Type.kind t
      | Mode : mode loc -> Type.mode t
      | Modality : modality loc -> Type.modality t
      | Alloc : Type.alloc t
      | Synchro : Type.synchro t
  end

  module Type_error = struct
    type t =
      | Type_mismatch :
          { kind : string
          ; sexp_of_kind : 'value -> Sexp.t
          ; value : 'value
          ; expected_type : _ Type.t
          ; expected_sets : (string, _) Typed.Expression.allow_set option
          ; hint : string option
          }
          -> t
      | Tuple_length_mismatch :
          { kind : string
          ; sexp_of_kind : 'value -> Sexp.t
          ; value : 'value
          ; expected_type : _ Type.t
          }
          -> t
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

    module Non_tuple : sig
      include module type of struct
        include Non_tuple
      end
    end

    val sexp_of_t : _ t -> Sexp.t
    val kind : kind t
    val mode : mode t
    val modality : modality t
    val alloc : alloc t
    val synchro : synchro t
    val tuple2 : 'a t -> 'b t -> ('a * ('b * unit)) t
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

      val compare : t -> t -> int
      val sexp_of_t : t -> Sexp.t
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

    module Axis : sig
      include module type of struct
        include Axis
      end

      module Map : Map.S with type key := packed

      val of_type : 'a Type.non_tuple Type.t -> 'a Type.non_tuple t
      val is_set : 'a t -> bool

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

        module Or_unrecognized : sig
          include module type of struct
            include Or_unrecognized
          end
        end

        module Map : Stdlib.Map.S with type key := packed

        val of_identifier : 'a Type.non_tuple Identifier.t -> 'a Type.non_tuple t
        val of_value : 'a Type.non_tuple Value.t -> 'a Type.non_tuple t
      end

      module Namespace : sig
        include module type of struct
          include Namespace
        end

        val sexp_of_t : _ t -> Sexp.t
        val same_namespace : 'a t -> 'b t -> bool
        val of_value : is_set:bool -> 'a Value.t -> 'a t
      end
    end

    module Identifier : sig
      include module type of struct
        include Identifier
      end

      val equal_witness : 'a t -> 'b t -> ('a, 'b) Stdlib.Type.eq option
    end

    module Value : sig
      include module type of struct
        include Value
      end

      val compare : 'a t -> 'a t -> int
      val sexp_of_t : _ t -> Sexp.t

      (** Checks whether a value is a default value for its axis. When not using
          [.explicit] or [.explicit_plus_unmangled] attributes, we leave identifiers
          untemplated over a particular axis [is_default] holds for all values in the
          mangling set for that axis.

          Returns [Actual_default] for values representing actual OxCaml defaults. These
          are the modes, kinds, etc. that OxCaml infers when there are no other
          annotations or constraints.

          Returns [Default_for_standard_mangling] for the kind [value_or_null], which is
          mangled as if it were default for standard "poly" attributes like
          [[@@kind k = (value_or_null, ...)]]. It is not mangled as default for
          [[@@kind.explicit_plus_unmangled ...]].

          Returns [Not_a_default] for all other values. *)
      val defaultness : 'a Type.non_tuple t -> Defaultness.t

      val as_expression : 'a t -> ('a, Expression.singleton) Expression.t

      (** Convert a value in the template language to a concrete OCaml AST node. *)
      val to_node : 'a Type.non_tuple t -> loc:location -> 'a Type.non_tuple Node.t
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

      val type_ : ('a, _) t -> 'a Type.t
      val to_set : ('a, _) t -> ('a, set) t
      val untype : ('a, 'allow_set) t -> Untyped.Expression.t

      val type_check
        :  Untyped.Expression.t
        -> expected:'a Type.t
        -> allow_set:(string, 's) allow_set
        -> (('a, 's) t, Type_error.t) result
    end
  end

  module Type_error : sig
    include module type of struct
      include Type_error
    end

    val to_error : loc:Location.t -> t -> Syntax_error.t

    val lift_to_error_result
      :  loc:Location.t
      -> ('a, t) result
      -> ('a, Syntax_error.t) result
  end
end
