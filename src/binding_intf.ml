open! Stdppx
open! Import

module Definitions = struct
  module type S = sig
    type identifier
    type node
    type t

    val of_identifier : identifier -> t
    val compare : t -> t -> int
    val pattern : unit -> (expression, t -> 'a, 'a) Ast_pattern.t

    (** Some bindings, notably product kinds, may recursively contain other identifiers,
        which must also be resolved. If there is no such identifier in the current
        environment, we assume it is a constant and rely on the compiler to complain if it
        is an unrecognized kind/mode, rather than check ourselves. *)
    val resolve : t -> find_identifier:(identifier -> t option) -> t

    (** Turn a binding into its corresponding AST node. *)
    val to_node : loc:location -> t -> node

    (** Turn this binding into something resembling an identifier. For modes and kind
        abbreviations, this is just the identity function. For product kinds, we turn e.g.
        [k1 & k2] into "'k1_k2'". The single quotes are permissible for interpolation of
        mangled function names, and resolve ambiguity between nested products. *)
    val to_mangled_identifier : t -> identifier
  end

  type ('a, 'id, 'node) t =
    (module S with type t = 'a and type identifier = 'id and type node = 'node)
end

module type Binding = sig
  include module type of struct
    include Definitions
  end

  module Kind :
    S with type identifier := Identifier.Kind.t and type node = jkind_annotation

  module Mode :
    S
    with type t = Identifier.Mode.t
     and type identifier := Identifier.Mode.t
     and type node = mode

  module Modality :
    S
    with type t = Identifier.Modality.t
     and type identifier := Identifier.Modality.t
     and type node = modality

  val kind : (Kind.t, Identifier.Kind.t, jkind_annotation) t
  val mode : (Mode.t, Identifier.Mode.t, mode) t
  val modality : (Modality.t, Identifier.Modality.t, modality) t
end
