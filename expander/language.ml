open! Stdppx
open! Import
include Language_intf.Definitions
open Result.Let_syntax

module Type = struct
  include Type

  let equal_witness_non_tuple
    : type a b. a non_tuple -> b non_tuple -> (a, b) Stdlib.Type.eq option
    =
    fun t1 t2 ->
    match t1, t2 with
    | Kind, Kind -> Some Equal
    | Mode, Mode -> Some Equal
    | Modality, Modality -> Some Equal
    | Alloc, Alloc -> Some Equal
    | Synchro, Synchro -> Some Equal
    | (Kind | Mode | Modality | Alloc | Synchro), _ -> None
  ;;

  let sexp_of_non_tuple : type a. a non_tuple -> Sexp.t = function
    | Kind -> Atom "Kind"
    | Mode -> Atom "Mode"
    | Modality -> Atom "Modality"
    | Alloc -> Atom "Alloc"
    | Synchro -> Atom "Synchro"
  ;;

  let rec equal_witness : type a b. a t -> b t -> (a, b) Stdlib.Type.eq option =
    fun t1 t2 ->
    match t1, t2 with
    | Non_tuple b1, Non_tuple b2 ->
      (match equal_witness_non_tuple b1 b2 with
       | Some Equal -> Some Equal
       | None -> None)
    | Tuple tp1, Tuple tp2 ->
      (match equal_tuple_witness tp1 tp2 with
       | None -> None
       | Some Equal -> Some Equal)
    | (Non_tuple _ | Tuple _), _ -> None

  and equal_tuple_witness : type a b. a tuple -> b tuple -> (a, b) Stdlib.Type.eq option =
    fun tp1 tp2 ->
    match tp1, tp2 with
    | [], [] -> Some Equal
    | hd1 :: tl1, hd2 :: tl2 ->
      (match equal_witness hd1 hd2 with
       | None -> None
       | Some Equal ->
         (match equal_tuple_witness tl1 tl2 with
          | None -> None
          | Some Equal -> Some Equal))
    | [], _ :: _ | _ :: _, [] -> None
  ;;

  let rec sexp_of_t : type a. a t -> Sexp.t = function
    | Non_tuple non_tuple -> sexp_of_non_tuple non_tuple
    | Tuple [ (Non_tuple Alloc as t1); (Non_tuple Mode as t2) ] ->
      List [ sexp_of_t t1; Atom "@"; sexp_of_t t2 ]
    | Tuple [ (Non_tuple Synchro as t1); (Non_tuple Mode as t2) ] ->
      List [ sexp_of_t t1; Atom "@"; sexp_of_t t2 ]
    | Tuple tp -> List (Atom "Tuple" :: sexp_of_tuple tp)

  and sexp_of_tuple : type a. a tuple -> Sexp.t list = function
    | [] -> []
    | hd :: tl -> sexp_of_t hd :: sexp_of_tuple tl
  ;;

  let compare_packed : packed -> packed -> int = Poly.compare
  let sexp_of_packed (P t) = sexp_of_t t
  let kind = Non_tuple Kind
  let mode = Non_tuple Mode
  let modality = Non_tuple Modality
  let alloc = Non_tuple Alloc
  let synchro = Non_tuple Synchro
  let tuple2 t1 t2 = Tuple [ t1; t2 ]
end

module Untyped = struct
  include Untyped

  module Axis = struct
    include Axis

    let compare : t -> t -> int = Poly.compare

    module Map = Map.Make (struct
        type nonrec t = t

        let compare = compare
      end)
  end

  module Identifier = struct
    include Identifier

    let compare t1 t2 = String.compare t1.ident t2.ident
    let sexp_of_t t = Atom t.ident
  end

  module Value = struct
    include Value

    let rec compare : t -> t -> int =
      fun t1 t2 ->
      match t1, t2 with
      | Identifier ident1, Identifier ident2 -> Identifier.compare ident1 ident2
      | Kind_product kinds1, Kind_product kinds2 ->
        Nonempty_list.compare kinds1 kinds2 ~cmp:compare
      | Kind_mod (kind1, mods1), Kind_mod (kind2, mods2) ->
        (match compare kind1 kind2 with
         | 0 ->
           let mods1 = Nonempty_list.sort_uniq ~cmp:compare mods1 in
           let mods2 = Nonempty_list.sort_uniq ~cmp:compare mods2 in
           Nonempty_list.compare ~cmp:compare mods1 mods2
         | n -> n)
      | Tuple ts1, Tuple ts2 -> Nonempty_list.compare ~cmp:compare ts1 ts2
      | Identifier _, _ -> -1
      | _, Identifier _ -> 1
      | Kind_product _, _ -> -1
      | _, Kind_product _ -> 1
      | Kind_mod _, _ -> -1
      | _, Kind_mod _ -> 1
      | Tuple _, _ -> .
      | _, Tuple _ -> .
    ;;

    let rec sexp_of_t : t -> Sexp.t = function
      | Identifier ident -> Identifier.sexp_of_t ident
      | Kind_product kinds ->
        List [ Atom "Product"; kinds |> Nonempty_list.sexp_of_t sexp_of_t ]
      | Kind_mod (kind, mods) ->
        List
          [ Atom "Mod"
          ; sexp_of_t kind
          ; mods
            |> Nonempty_list.sort_uniq ~cmp:compare
            |> Nonempty_list.sexp_of_t sexp_of_t
          ]
      | Tuple ts ->
        List (Atom "Tuple" :: (ts |> Nonempty_list.to_list |> List.map ~f:sexp_of_t))
    ;;
  end

  module Pattern = struct
    include Pattern

    let rec compare : t -> t -> int =
      fun t1 t2 ->
      match t1, t2 with
      | Wildcard, Wildcard -> 0
      | Identifier ident1, Identifier ident2 -> Identifier.compare ident1 ident2
      | Tuple ts1, Tuple ts2 -> Nonempty_list.compare ~cmp:compare ts1 ts2
      | Wildcard, _ -> -1
      | _, Wildcard -> 1
      | Identifier _, _ -> -1
      | _, Identifier _ -> 1
      | Tuple _, _ -> .
      | _, Tuple _ -> .
    ;;

    let rec sexp_of_t : t -> Sexp.t = function
      | Wildcard -> Atom "Wildcard"
      | Identifier ident -> Identifier.sexp_of_t ident
      | Tuple ts ->
        List (Atom "Tuple" :: (ts |> Nonempty_list.to_list |> List.map ~f:sexp_of_t))
    ;;
  end

  module Expression = struct
    include Expression

    let rec compare : t -> t -> int =
      fun t1 t2 ->
      match t1, t2 with
      | Identifier ident1, Identifier ident2 -> Identifier.compare ident1 ident2
      | Kind_product kinds1, Kind_product kinds2 ->
        Nonempty_list.compare kinds1 kinds2 ~cmp:compare
      | Kind_mod (kind1, mods1), Kind_mod (kind2, mods2) ->
        (match compare kind1 kind2 with
         | 0 ->
           let mods1 = Nonempty_list.sort_uniq ~cmp:compare mods1 in
           let mods2 = Nonempty_list.sort_uniq ~cmp:compare mods2 in
           Nonempty_list.compare ~cmp:compare mods1 mods2
         | n -> n)
      | Kind_coercion (kind1, coerce_to1), Kind_coercion (kind2, coerce_to2) ->
        (match compare kind1 kind2 with
         | 0 -> compare coerce_to1 coerce_to2
         | n -> n)
      | Comma_separated ts1, Comma_separated ts2 ->
        Nonempty_list.compare ~cmp:compare ts1 ts2
      | Typed (t1, typ1), Typed (t2, typ2) ->
        (match compare t1 t2 with
         | 0 -> Type.compare_packed typ1 typ2
         | n -> n)
      | Identifier _, _ -> -1
      | _, Identifier _ -> 1
      | Kind_product _, _ -> -1
      | _, Kind_product _ -> 1
      | Kind_mod _, _ -> -1
      | _, Kind_mod _ -> 1
      | Kind_coercion _, _ -> -1
      | _, Kind_coercion _ -> 1
      | Comma_separated _, _ -> -1
      | _, Comma_separated _ -> 1
      | Typed _, _ -> .
      | _, Typed _ -> .
    ;;

    let rec sexp_of_t : t -> Sexp.t = function
      | Identifier ident -> Identifier.sexp_of_t ident
      | Kind_product kinds ->
        List (Atom "Product" :: (kinds |> Nonempty_list.to_list |> List.map ~f:sexp_of_t))
      | Kind_mod (kind, mods) ->
        List
          [ Atom "Mod"
          ; sexp_of_t kind
          ; mods
            |> Nonempty_list.sort_uniq ~cmp:compare
            |> Nonempty_list.sexp_of_t sexp_of_t
          ]
      | Kind_coercion (kind, coerce_to) ->
        List [ Atom "Kind_coercion"; sexp_of_t kind; sexp_of_t coerce_to ]
      | Comma_separated ts ->
        List
          (Atom "Comma_separated" :: (ts |> Nonempty_list.to_list |> List.map ~f:sexp_of_t)
          )
      | Typed (t, typ) -> List [ sexp_of_t t; Atom ":"; Type.sexp_of_packed typ ]
    ;;
  end
end

module Typed = struct
  include Typed

  module Vec = struct
    (* Used in some logic below to make error handling easier. *)

    type ('item, 'length) t =
      | [] : (_, unit) t
      | ( :: ) : 'item * ('item, 'length) t -> ('item, _ * 'length) t

    type wrong_length = Wrong_length

    let rec of_list_with_length
      : type item length.
        item list -> length Type.tuple -> ((item, length) t, wrong_length) result
      =
      fun items length ->
      match items, length with
      | [], [] -> Ok []
      | hd_items :: tl_items, _ :: tl_length ->
        let* tl = of_list_with_length tl_items tl_length in
        Ok (hd_items :: tl)
      | _ :: _, [] | [], _ :: _ -> Error Wrong_length
    ;;
  end

  module Axis = struct
    include Axis

    let compare_packed : packed -> packed -> int = Poly.compare

    let of_type : type a. a Type.non_tuple Type.t -> a Type.non_tuple t = function
      | Non_tuple Kind -> Singleton Kind
      | Non_tuple Mode -> Singleton Mode
      | Non_tuple Modality -> Singleton Modality
      | Non_tuple Alloc -> Singleton Alloc
      | Non_tuple Synchro -> Singleton Synchro
    ;;

    let is_set : type a. a t -> bool = function
      | Set _ -> true
      | Singleton _ -> false
    ;;

    module Map = Map.Make (struct
        type t = packed

        let compare = compare_packed
      end)

    module Sub_axis = struct
      include Sub_axis

      module Modal = struct
        include Modal

        let of_mode_identifier : string -> t Or_unrecognized.t = function
          | "global" | "local" -> Known Locality
          | "nonportable" | "shareable" | "corruptible" | "portable" -> Known Portability
          | "uncontended" | "contended" | "corrupted" | "shared" -> Known Contention
          | "stateful" | "reading" | "writing" | "stateless" -> Known Statefulness
          | "read_write" | "read" | "write" | "immutable" -> Known Visibility
          | "many" | "once" -> Known Linearity
          | "aliased" | "unique" -> Known Uniqueness
          | "unyielding" | "yielding" -> Known Yielding
          | "forkable" | "unforkable" -> Known Forkable
          | _ -> Unrecognized
        ;;

        let sexp_of_t = function
          | Locality -> Atom "Locality"
          | Portability -> Atom "Portability"
          | Contention -> Atom "Contention"
          | Statefulness -> Atom "Statefulness"
          | Visibility -> Atom "Visibility"
          | Linearity -> Atom "Linearity"
          | Uniqueness -> Atom "Uniqueness"
          | Yielding -> Atom "Yielding"
          | Forkable -> Atom "Forkable"
        ;;
      end

      module Mode = struct
        include Mode

        let default (Mode m) =
          match m with
          | Locality -> "global"
          | Portability -> "nonportable"
          | Contention -> "uncontended"
          | Statefulness -> "stateful"
          | Visibility -> "read_write"
          | Linearity -> "many"
          | Uniqueness -> "aliased"
          | Yielding -> "unyielding"
          | Forkable ->
            (* Above comment also applies to [forkable]. *)
            "forkable"
        ;;

        let of_mode_identifier str : _ Or_unrecognized.t =
          match Modal.of_mode_identifier str with
          | Known m -> Known (Mode m)
          | Unrecognized -> Unrecognized
        ;;

        let sexp_of_t (Mode m) = Modal.sexp_of_t m
      end

      module Modality = struct
        include Modality

        let default (Modality m) =
          match m with
          | Locality -> "local"
          | Portability -> "nonportable"
          | Contention -> "uncontended"
          | Statefulness -> "stateful"
          | Visibility -> "read_write"
          | Linearity -> "once"
          | Uniqueness -> "unique"
          | Yielding -> "yielding"
          | Forkable -> "unforkable"
        ;;

        let of_mode_identifier str : _ Or_unrecognized.t =
          match Modal.of_mode_identifier str with
          | Known m -> Known (Modality m)
          | Unrecognized -> Unrecognized
        ;;

        let sexp_of_t (Modality m) = Modal.sexp_of_t m
      end

      module Or_unrecognized = struct
        include Or_unrecognized

        let sexp_of_t sexp_of_a = function
          | Known a -> sexp_of_a a
          | Unrecognized -> Atom "unrecognized"
        ;;
      end

      let compare_packed : packed -> packed -> int = Poly.compare

      module Map = Stdlib.Map.Make (struct
          type t = packed

          let compare = compare_packed
        end)

      let of_identifier : type a. a Type.non_tuple Identifier.t -> a Type.non_tuple t =
        fun { ident; type_ = Non_tuple type_ } ->
        match type_ with
        | Kind -> Kind
        | Mode -> Mode (Mode.of_mode_identifier ident)
        | Modality -> Modality (Modality.of_mode_identifier ident)
        | Alloc -> Alloc
        | Synchro -> Synchro
      ;;

      let of_value : type a. a Type.non_tuple Value.t -> a Type.non_tuple t = function
        | Identifier ident -> of_identifier ident
        | Kind_product _ -> Kind
        | Kind_mod _ -> Kind
      ;;

      let default : type a. a Type.non_tuple t -> string Or_unrecognized.t = function
        | Kind -> Known "value"
        | Mode (Known mode) -> Known (Mode.default mode)
        | Modality (Known modality) -> Known (Modality.default modality)
        | Mode Unrecognized | Modality Unrecognized -> Unrecognized
        | Alloc -> Known "heap"
        | Synchro -> Known "unsync"
      ;;

      let sexp_of_t : type a. a t -> Sexp.t = function
        | Kind -> Atom "Kind"
        | Mode m -> List [ Atom "Mode"; Or_unrecognized.sexp_of_t Mode.sexp_of_t m ]
        | Modality m ->
          List [ Atom "Modality"; Or_unrecognized.sexp_of_t Modality.sexp_of_t m ]
        | Alloc -> Atom "Alloc"
        | Synchro -> Atom "Synchro"
      ;;
    end

    module Namespace = struct
      include Namespace

      let rec of_value : type a. is_set:bool -> a Value.t -> a t =
        fun ~is_set val_ ->
        let to_namespace sub_axis = if is_set then Set sub_axis else Singleton sub_axis in
        match val_ with
        | Identifier _ -> to_namespace (Sub_axis.of_value val_)
        | Kind_product _ -> to_namespace (Sub_axis.of_value val_)
        | Kind_mod _ -> to_namespace (Sub_axis.of_value val_)
        | Tuple tp -> Tuple (of_value_tuple ~is_set tp)

      and of_value_tuple : type a. is_set:bool -> a Value.tuple -> a tuple =
        fun ~is_set tp ->
        match tp with
        | [] -> []
        | hd :: tl -> of_value ~is_set hd :: of_value_tuple ~is_set tl
      ;;

      (* equality modulo mode/modality conflation *)
      let rec same_namespace : type a b. a t -> b t -> bool =
        fun tp1 tp2 ->
        match tp1, tp2 with
        | Singleton tp1, Singleton tp2 | Set tp1, Set tp2 ->
          (* We conflate modalities and modes *)
          (match tp1, tp2 with
           | Mode (Known (Mode m1)), Modality (Known (Modality m2)) -> m1 = m2
           | Modality (Known (Modality m1)), Mode (Known (Mode m2)) -> m1 = m2
           | Mode Unrecognized, Modality Unrecognized
           | Modality Unrecognized, Mode Unrecognized -> true
           | _, _ -> Sub_axis.compare_packed (P tp1) (P tp2) = 0)
        | Tuple tp1, Tuple tp2 -> same_namespace_tuple tp1 tp2
        | (Singleton _ | Set _), Tuple _
        | Tuple _, (Singleton _ | Set _)
        | Singleton _, Set _
        | Set _, Singleton _ -> false

      and same_namespace_tuple : type a b. a tuple -> b tuple -> bool =
        fun tp1 tp2 ->
        match tp1, tp2 with
        | [], [] -> true
        | hd1 :: tl1, hd2 :: tl2 -> same_namespace hd1 hd2 && same_namespace_tuple tl1 tl2
        | [], _ :: _ | _ :: _, [] -> false
      ;;

      let rec sexp_of_t : type a. a t -> Sexp.t = function
        | Singleton t -> Sub_axis.sexp_of_t t
        | Set t -> List [ Atom "Set"; Sub_axis.sexp_of_t t ]
        | Tuple tp -> List (Atom "Tuple" :: sexp_of_tuple tp)

      and sexp_of_tuple : type a. a tuple -> Sexp.t list = function
        | [] -> []
        | hd :: tl -> sexp_of_t hd :: sexp_of_tuple tl
      ;;
    end
  end

  module Identifier = struct
    include Identifier

    let equal_witness { type_; ident } t =
      match Type.equal_witness type_ t.type_ with
      | Some _ as eq when String.equal ident t.ident -> eq
      | _ -> None
    ;;
  end

  module Value = struct
    include Value

    let rec untype : type a. a t -> Untyped.Value.t = function
      | Identifier { ident; type_ = _ } -> Identifier { ident }
      | Kind_product kinds -> Kind_product (Nonempty_list.map kinds ~f:untype)
      | Kind_mod (kind, mods) -> Kind_mod (untype kind, Nonempty_list.map mods ~f:untype)
      | Tuple tp -> Tuple (untype_tuple tp)

    and untype_tuple : type a b. (a * b) tuple -> Untyped.Value.t Nonempty_list.t
      = function
      | [ hd ] -> Nonempty_list.singleton (untype hd)
      | hd :: (_ :: _ as tl) -> Nonempty_list.cons (untype hd) (untype_tuple tl)
    ;;

    let compare t1 t2 = Untyped.Value.compare (untype t1) (untype t2)
    let sexp_of_t t = Untyped.Value.sexp_of_t (untype t)

    (* Translate a value back into the expression that most directly corresponds to it.
       More exactly, [eval (as_expression t) === t] when evaluating in an empty
       environment. *)
    let rec as_expression : type a. a t -> (a, Expression.singleton) Expression.t
      = function
      | Identifier ident -> Identifier ident
      | Kind_mod (kind, mods) ->
        Kind_mod (as_expression kind, Nonempty_list.map mods ~f:as_expression)
      | Kind_product kinds -> Kind_product (Nonempty_list.map kinds ~f:as_expression)
      | Tuple tp -> Tuple (uneval_tuple tp)

    and uneval_tuple : type a. a tuple -> a Expression.tuple = function
      | [] -> []
      | hd :: tl -> as_expression hd :: uneval_tuple tl
    ;;

    let is_actual_default : type a. a Type.non_tuple t -> bool = function
      | Identifier ident ->
        (match Axis.Sub_axis.default (Axis.Sub_axis.of_identifier ident) with
         | Known default -> String.equal ident.ident default
         | Unrecognized -> false)
      | Kind_product _ | Kind_mod _ -> false
    ;;

    let is_default_for_standard_mangling : type a. a Type.non_tuple t -> bool = function
      | Identifier { type_ = Non_tuple Kind; ident = "value_or_null" } -> true
      | _ -> false
    ;;

    let defaultness : type a. a Type.non_tuple t -> Defaultness.t =
      fun value ->
      if is_actual_default value
      then Actual_default
      else if is_default_for_standard_mangling value
      then Default_for_standard_mangling
      else Not_a_default
    ;;

    let rec to_node
      : type a. a Type.non_tuple t -> loc:location -> a Type.non_tuple Node.t
      =
      fun t ~loc ->
      match t with
      | Identifier { ident; type_ } ->
        (match type_ with
         | Non_tuple Mode -> Mode { txt = Mode ident; loc }
         | Non_tuple Modality -> Modality { txt = Modality ident; loc }
         | Non_tuple Kind ->
           Jkind_annotation
             { pjka_desc = Pjk_abbreviation ({ txt = Lident ident; loc }, [])
             ; pjka_loc = loc
             }
         | Non_tuple Alloc -> Alloc
         | Non_tuple Synchro -> Synchro)
      | Kind_product kinds ->
        let ghost_loc = { loc with loc_ghost = true } in
        let kinds =
          kinds
          |> Nonempty_list.to_list
          |> List.map ~f:(fun kind ->
            let (Jkind_annotation kind) = to_node ~loc:ghost_loc kind in
            kind)
        in
        Jkind_annotation { pjka_desc = Pjk_product kinds; pjka_loc = loc }
      | Kind_mod (kind, mods) ->
        let ghost_loc = { loc with loc_ghost = true } in
        let (Jkind_annotation kind) = to_node ~loc:ghost_loc kind in
        let mods =
          mods
          |> Nonempty_list.to_list
          |> List.sort_uniq ~cmp:compare
          |> List.map ~f:(fun (Identifier { ident; type_ = Non_tuple Modality }) ->
            { txt = Mode ident; loc = ghost_loc })
        in
        Jkind_annotation { pjka_desc = Pjk_mod (kind, mods); pjka_loc = loc }
    ;;
  end

  module Pattern = struct
    include Pattern

    let rec untype : type a. a t -> Untyped.Pattern.t = function
      | Wildcard -> Wildcard
      | Identifier { ident; type_ = _ } -> Identifier { ident }
      | Tuple tp -> Tuple (untype_tuple tp)

    and untype_tuple : type a b. (a * b) tuple -> Untyped.Pattern.t Nonempty_list.t
      = function
      | [ hd ] -> Nonempty_list.singleton (untype hd)
      | hd :: (_ :: _ as tl) -> Nonempty_list.cons (untype hd) (untype_tuple tl)
    ;;

    let rec type_check
      : type a. Untyped.Pattern.t -> expected:a Type.t -> (a t, Type_error.t) result
      =
      fun untyped ~expected ->
      match untyped, expected with
      | Wildcard, _ -> Ok Wildcard
      | Identifier { ident }, type_ -> Ok (Identifier { ident; type_ })
      | Tuple untyped_tp, Tuple type_tp ->
        let* vec =
          match Vec.of_list_with_length (Nonempty_list.to_list untyped_tp) type_tp with
          | Ok vec -> Ok vec
          | Error Wrong_length ->
            Error
              (Type_error.Tuple_length_mismatch
                 { kind = "pattern"
                 ; sexp_of_kind = Untyped.Pattern.sexp_of_t
                 ; value = Tuple untyped_tp
                 ; expected_type = Tuple type_tp
                 })
        in
        let+ tuple = type_check_tuple vec ~expected:type_tp in
        Tuple tuple
      | (Tuple _ as node), (Non_tuple _ as type_) ->
        Error
          (Type_mismatch
             { kind = "pattern"
             ; sexp_of_kind = Untyped.Pattern.sexp_of_t
             ; value = node
             ; expected_type = type_
             ; expected_sets = None
             ; hint = None
             })

    and type_check_tuple
      : type a.
        (Untyped.Pattern.t, a) Vec.t
        -> expected:a Type.tuple
        -> (a tuple, Type_error.t) result
      =
      fun untyped ~expected ->
      match untyped, expected with
      | [], [] -> Ok []
      | untyped_hd :: untyped_tl, type_hd :: type_tl ->
        let* hd = type_check untyped_hd ~expected:type_hd in
        let+ tl = type_check_tuple untyped_tl ~expected:type_tl in
        hd :: tl
    ;;
  end

  module Expression = struct
    include Expression

    let sexp_of_sets : type s. (string, s) allow_set -> Sexp.t = function
      | Singleton_only _hint -> Atom "singleton"
      | Set_or_singleton -> Atom "set with unions"
    ;;

    let rec type_ : type a is_set. (a, is_set) t -> a Type.t = function
      | Identifier { type_; _ } -> type_
      | Kind_mod _ -> Type.kind
      | Kind_product _ -> Type.kind
      | Kind_coercion _ -> Type.kind
      | Tuple tp -> Tuple (type_tuple tp)
      | Union (hd :: _) -> type_ hd

    and type_tuple : type a b. (a * b) tuple -> (a * b) Type.tuple = function
      | [ hd ] -> [ type_ hd ]
      | hd :: (_ :: _ as tl) -> type_ hd :: type_tuple tl
    ;;

    let rec untype : type a is_set. (a, is_set) t -> Untyped.Expression.t = function
      | Identifier { ident; type_ = _ } -> Identifier { ident }
      | Kind_mod (kind, mods) -> Kind_mod (untype kind, Nonempty_list.map mods ~f:untype)
      | Kind_product kinds -> Kind_product (Nonempty_list.map kinds ~f:untype)
      | Kind_coercion (kind, coerce_to) -> Kind_coercion (untype kind, untype coerce_to)
      | Tuple tp -> Comma_separated (untype_tuple tp)
      | Union ts -> Comma_separated (Nonempty_list.map ts ~f:untype)

    and untype_tuple : type a b. (a * b) tuple -> Untyped.Expression.t Nonempty_list.t
      = function
      | [ hd ] -> [ untype hd ]
      | hd :: (_ :: _ as tl) -> untype hd :: Nonempty_list.to_list (untype_tuple tl)
    ;;

    let rec to_set : type a is_set. (a, is_set) t -> (a, set) t = function
      | Identifier ident -> Identifier ident
      | Kind_mod (kind, mods) -> Kind_mod (to_set kind, mods)
      | Kind_product kinds -> Kind_product (Nonempty_list.map kinds ~f:to_set)
      | Kind_coercion (kind, coerce_to) -> Kind_coercion (to_set kind, coerce_to)
      | Tuple tp -> Tuple tp
      | Union ts -> Union (Nonempty_list.map ts ~f:to_set)
    ;;

    let type_mismatch
      (type s)
      ?hint
      ~value
      ~expected
      ~(allow_set : (string, s) Expression.allow_set)
      ()
      =
      Error
        (Type_error.Type_mismatch
           { kind = "expression"
           ; sexp_of_kind = Untyped.Expression.sexp_of_t
           ; value
           ; expected_type = expected
           ; expected_sets = Some allow_set
           ; hint
           })
    ;;

    let rec type_check
      : type a s.
        Untyped.Expression.t
        -> expected:a Type.t
        -> allow_set:(string, s) allow_set
        -> ((a, s) t, Type_error.t) result
      =
      fun untyped ~expected ~allow_set ->
      match untyped, expected with
      | Identifier { ident }, type_ -> Ok (Identifier { ident; type_ })
      | Kind_product kinds, Non_tuple Kind ->
        let+ kinds =
          Nonempty_list.Or_first_error.map
            kinds
            ~f:(type_check ~expected:(Non_tuple Kind) ~allow_set)
        in
        Kind_product kinds
      | (Kind_product _ as value), expected ->
        type_mismatch ~value ~expected ~allow_set ()
      | Kind_mod (kind, mods), Non_tuple Kind ->
        let* kind = type_check kind ~expected:(Non_tuple Kind) ~allow_set in
        let+ mods =
          Nonempty_list.Or_first_error.map
            mods
            ~f:
              (type_check
                 ~expected:(Non_tuple Modality)
                 ~allow_set:
                   (Singleton_only
                      { why_no_set = "sets not allowed inside [kind mod] modalities" }))
        in
        Kind_mod (kind, mods)
      | Kind_coercion (kind, coerce_to), Non_tuple Kind ->
        let* kind = type_check kind ~expected ~allow_set in
        let+ coerce_to = type_check coerce_to ~expected ~allow_set:Set_or_singleton in
        Kind_coercion (kind, coerce_to)
      | (Kind_coercion _ as value), expected ->
        type_mismatch ~value ~expected ~allow_set ()
      | (Kind_mod _ as value), expected -> type_mismatch ~value ~expected ~allow_set ()
      | Comma_separated untyped_tp, Tuple type_tp ->
        let* vec =
          match Vec.of_list_with_length (Nonempty_list.to_list untyped_tp) type_tp with
          | Ok vec -> Ok vec
          | Error Wrong_length ->
            Error
              (Type_error.Tuple_length_mismatch
                 { kind = "expression"
                 ; sexp_of_kind = Untyped.Expression.sexp_of_t
                 ; value = untyped
                 ; expected_type = expected
                 })
        in
        let+ tuple = type_check_tuple vec ~expected:type_tp in
        Tuple tuple
      | Comma_separated untyped_union, Non_tuple _ ->
        (match allow_set with
         | Singleton_only { why_no_set = hint } ->
           type_mismatch ~hint ~value:untyped ~expected ~allow_set ()
         | Set_or_singleton ->
           let+ subsets =
             Nonempty_list.Or_first_error.map
               untyped_union
               ~f:(type_check ~expected ~allow_set:Set_or_singleton)
           in
           Union (subsets :> (_, set) t Nonempty_list.t))
      | (Typed (untyped, P typ) as value), expected ->
        (match Type.equal_witness typ expected with
         | None -> type_mismatch ~value ~expected ~allow_set ()
         | Some Equal -> type_check untyped ~expected ~allow_set)

    and type_check_tuple
      : type a.
        (Untyped.Expression.t, a) Vec.t
        -> expected:a Type.tuple
        -> (a tuple, Type_error.t) result
      =
      fun untyped ~expected ->
      match untyped, expected with
      | [], [] -> Ok []
      | untyped_hd :: untyped_tl, type_hd :: type_tl ->
        let* hd =
          type_check
            untyped_hd
            ~expected:type_hd
            ~allow_set:(Singleton_only { why_no_set = "sets not allowed inside tuples" })
        in
        let+ tl = type_check_tuple untyped_tl ~expected:type_tl in
        hd :: tl
    ;;
  end
end

module Type_error = struct
  include Type_error

  let sexp_of_t : t -> Sexp.t = function
    | Type_mismatch
        { kind; sexp_of_kind; value; expected_type = type_; expected_sets = is_set; hint }
      ->
      Sexplib0.Sexp.message
        "Type mismatch"
        ([ "kind", Atom kind
         ; "value", sexp_of_kind value
         ; "expected type", Type.sexp_of_t type_
         ]
         @ (match is_set with
            | Some is_set -> [ "expected sets", Typed.Expression.sexp_of_sets is_set ]
            | None -> [])
         @
         match hint with
         | Some hint -> [ "hint", Atom hint ]
         | None -> [])
    | Tuple_length_mismatch { kind; sexp_of_kind; value; expected_type = type_ } ->
      Sexplib0.Sexp.message
        "Tuple length mismatch"
        [ "kind", Atom kind
        ; "value", sexp_of_kind value
        ; "expected type", Type.sexp_of_t type_
        ]
  ;;

  let to_error ~loc t =
    Syntax_error.createf
      ~loc
      "%s"
      (Sexp.to_string_hum (List [ Atom "[%template]"; sexp_of_t t ]))
  ;;

  let lift_to_error_result ~loc = function
    | Ok _ as ok -> ok
    | Error type_error -> Error (to_error ~loc type_error)
  ;;
end

(* Note about conflating axes (e.g. in [Env.bind]):

   We always conflate modes and modalities for the portability and contention axes. These
   axes have the property that the legacy mode coincides with the top mode for comonadic
   axes and bottom mode for monadic axes[^0]. When this holds, ['a @ m -> 'b @ n] is
   equivalent to ['a @@ m -> 'b @@ n].

   More thoroughly: let [t @@ m] be a type whenever [t] is a type and [m] is a modality,
   such that [t @@ m] behaves like
   {[
     type t_atat_m = { inner : t } [@@unboxed]
   ]}
   i.e. is a zero-cost modality box around the type. Note that we define [t @@ m] even
   when [m] is a modality that does nothing; for example, [t @@ local] behaves just as [t]
   (since the [local] modality does nothing[^1]).

   Then, for all modes/modalities [m] and [n] on comonadic (resp. monadic) axes, if we let
   [ext_m] and [ext_n] be the top (resp. bottom) mode of the corresponding axes ([ext] as
   in "extremum"), then ['a @ m -> 'b @ n] is equivalent to
   ['a @@ m @ ext_m -> 'b @@ n @ ext_n]. For example, ['a @ local -> 'b @ global] is
   equivalent to ['a @@ local @ local -> 'b @@ global @ local], and (since ['a @@ local]
   is just ['a]) also ['a @ local -> 'b @@ global @ local].

   To make conflating a mode with its corresponding modality act in unsurprising ways, we
   want ['a @ m -> 'b @ n] to be equivalent to ['a @@ m -> 'b @@ n], which is implicitly
   ['a @@ m @ legacy_m -> 'b @@ n @ legacy_n]. This holds exactly when [ext_m = legacy_m]
   and [ext_n = legacy_n].

   Going through all of the currently supported axes:
   {v
      +-------------+-------------+-------------+-------------+
      |    axis     |  direction  |   [ext_m]   |   legacy    |
      +-------------+-------------+-------------+-------------+
      | locality    | comonadic   | local       | global      |
      +-------------+-------------+-------------+-------------+
      | portability | comonadic   | nonportable | nonportable |
      +-------------+-------------+-------------+-------------+
      | contention  |   monadic   | uncontended | uncontended |
      +-------------+-------------+-------------+-------------+
      | visibility  | comonadic   | stateful    | stateful    |
      +-------------+-------------+-------------+-------------+
      | access      |   monadic   | read_write  | read_write  |
      +-------------+-------------+-------------+-------------+
      | affinity    | comonadic   | once        | many        |
      +-------------+-------------+-------------+-------------+
      | uniqueness  |   monadic   | unique      | aliased     |
      +-------------+-------------+-------------+-------------+
      | yielding    | comonadic   | yielding    | unyielding  |
      +-------------+-------------+-------------+-------------+
      | forkable    | comonadic   | unforkable  | forkable    |
      +-------------+-------------+-------------+-------------+
   v}

   The only axes for which the right two columns align are portability and contention. For
   this reason, we always conflate modes and modalities on these two axes, and never on
   the other axes.

   [^0] The "top" (resp. "bottom") mode of an axis is the mode which is a super-mode
   (resp. sub-mode) of all other modes on the axis.

   [^1] Each modality acts as meet (min) for comonadic axes and join (max) for monadic
   axes; e.g. the [global] modality acts as [fun m -> meet global m]. This means the
   [local] modality acts as [fun m -> meet local m], but since [local] is top for the
   locality axis, [meet local m = m], so [local] just acts as [fun m -> m], i.e. does
   nothing.
*)
