open! Stdppx
open! Import
include Language_intf.Definitions

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
    let sexp_of_t t = Sexp.Atom t.ident
  end

  module Value = struct
    include Value

    let rec compare : t -> t -> int =
      fun t1 t2 ->
      match t1, t2 with
      | Identifier ident1, Identifier ident2 -> Identifier.compare ident1 ident2
      | Kind_product kinds1, Kind_product kinds2 ->
        List.compare kinds1 kinds2 ~cmp:compare
      | Kind_mod (kind1, mods1), Kind_mod (kind2, mods2) ->
        (match compare kind1 kind2 with
         | 0 ->
           let mods1 = List.sort_uniq ~cmp:compare mods1 in
           let mods2 = List.sort_uniq ~cmp:compare mods2 in
           List.compare ~cmp:compare mods1 mods2
         | n -> n)
      | Tuple ts1, Tuple ts2 -> List.compare ~cmp:compare ts1 ts2
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
      | Kind_product kinds -> List [ Atom "Product"; sexp_of_list sexp_of_t kinds ]
      | Kind_mod (kind, mods) ->
        List
          [ Atom "Mod"
          ; sexp_of_t kind
          ; List.sort_uniq mods ~cmp:compare |> sexp_of_list sexp_of_t
          ]
      | Tuple ts -> List (Sexp.Atom "Tuple" :: List.map ts ~f:sexp_of_t)
    ;;
  end

  module Pattern = struct
    include Pattern

    let rec compare : t -> t -> int =
      fun t1 t2 ->
      match t1, t2 with
      | Identifier ident1, Identifier ident2 -> Identifier.compare ident1 ident2
      | Tuple ts1, Tuple ts2 -> List.compare ~cmp:compare ts1 ts2
      | Identifier _, _ -> -1
      | _, Identifier _ -> 1
      | Tuple _, _ -> .
      | _, Tuple _ -> .
    ;;

    let rec sexp_of_t : t -> Sexp.t = function
      | Identifier ident -> Identifier.sexp_of_t ident
      | Tuple ts -> List (Sexp.Atom "Tuple" :: List.map ts ~f:sexp_of_t)
    ;;
  end

  module Expression = struct
    include Expression

    let rec compare : t -> t -> int =
      fun t1 t2 ->
      match t1, t2 with
      | Identifier ident1, Identifier ident2 -> Identifier.compare ident1 ident2
      | Kind_product kinds1, Kind_product kinds2 ->
        List.compare kinds1 kinds2 ~cmp:compare
      | Kind_mod (kind1, mods1), Kind_mod (kind2, mods2) ->
        (match compare kind1 kind2 with
         | 0 ->
           let mods1 = List.sort_uniq ~cmp:compare mods1 in
           let mods2 = List.sort_uniq ~cmp:compare mods2 in
           List.compare ~cmp:compare mods1 mods2
         | n -> n)
      | Tuple ts1, Tuple ts2 -> List.compare ~cmp:compare ts1 ts2
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
      | Kind_product kinds -> List (Atom "Product" :: List.map kinds ~f:sexp_of_t)
      | Kind_mod (kind, mods) ->
        List
          [ Atom "Mod"
          ; sexp_of_t kind
          ; List.sort_uniq mods ~cmp:compare |> sexp_of_list sexp_of_t
          ]
      | Tuple ts -> List (Sexp.Atom "Tuple" :: List.map ts ~f:sexp_of_t)
    ;;
  end
end

module Typed = struct
  include Typed

  module Type = struct
    include Type

    let equal_witness_basic : type a b. a basic -> b basic -> (a, b) Stdlib.Type.eq option
      =
      fun t1 t2 ->
      match t1, t2 with
      | Kind, Kind -> Some Equal
      | Mode, Mode -> Some Equal
      | Modality, Modality -> Some Equal
      | Alloc, Alloc -> Some Equal
      | (Kind | Mode | Modality | Alloc), _ -> None
    ;;

    let sexp_of_basic : type a. a basic -> Sexp.t = function
      | Kind -> Atom "Kind"
      | Mode -> Atom "Mode"
      | Modality -> Atom "Modality"
      | Alloc -> Atom "Alloc"
    ;;

    let rec equal_witness : type a b. a t -> b t -> (a, b) Stdlib.Type.eq option =
      fun t1 t2 ->
      match t1, t2 with
      | Basic b1, Basic b2 ->
        (match equal_witness_basic b1 b2 with
         | Some Equal -> Some Equal
         | None -> None)
      | Tuple tp1, Tuple tp2 ->
        (match equal_tuple_witness tp1 tp2 with
         | None -> None
         | Some Equal -> Some Equal)
      | (Basic _ | Tuple _), _ -> None

    and equal_tuple_witness : type a b. a tuple -> b tuple -> (a, b) Stdlib.Type.eq option
      =
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
      | Basic basic -> sexp_of_basic basic
      | Tuple [ (Basic Alloc as t1); (Basic Mode as t2) ] ->
        List [ sexp_of_t t1; Atom "@"; sexp_of_t t2 ]
      | Tuple tp -> List (Atom "Tuple" :: sexp_of_tuple tp)

    and sexp_of_tuple : type a. a tuple -> Sexp.t list = function
      | [] -> []
      | hd :: tl -> sexp_of_t hd :: sexp_of_tuple tl
    ;;

    let kind = Basic Kind
    let mode = Basic Mode
    let modality = Basic Modality
    let alloc = Basic Alloc
    let tuple2 t1 t2 = Tuple [ t1; t2 ]
  end

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
      let open Result.Let_syntax in
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

    let of_type : type a. a Type.basic Type.t -> a Type.basic t = function
      | Basic Kind -> Kind
      | Basic Mode -> Mode
      | Basic Modality -> Modality
      | Basic Alloc -> Alloc
      | Tuple _ -> .
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
          | "nonportable" | "portable" -> Known Portability
          | "uncontended" | "contended" | "shared" -> Known Contention
          | "stateful" | "observing" | "stateless" -> Known Visibility
          | "read_write" | "read" | "immutable" -> Known Access
          | "many" | "once" -> Known Affinity
          | "aliased" | "unique" -> Known Uniqueness
          | "unyielding" | "yielding" -> Known Yielding
          | _ -> Unrecognized
        ;;
      end

      module Mode = struct
        include Mode

        let default (Mode m) =
          match m with
          | Locality -> "global"
          | Portability -> "nonportable"
          | Contention -> "uncontended"
          | Visibility -> "stateful"
          | Access -> "read_write"
          | Affinity -> "many"
          | Uniqueness -> "aliased"
          | Yielding -> "unyielding"
        ;;

        let of_mode_identifier str : _ Or_unrecognized.t =
          match Modal.of_mode_identifier str with
          | Known m -> Known (Mode m)
          | Unrecognized -> Unrecognized
        ;;
      end

      module Modality = struct
        include Modality

        let default (Modality m) =
          match m with
          | Locality -> "local"
          | Portability -> "nonportable"
          | Contention -> "uncontended"
          | Visibility -> "stateful"
          | Access -> "read_write"
          | Affinity -> "once"
          | Uniqueness -> "unique"
          | Yielding -> "yielding"
        ;;

        let of_mode_identifier str : _ Or_unrecognized.t =
          match Modal.of_mode_identifier str with
          | Known m -> Known (Modality m)
          | Unrecognized -> Unrecognized
        ;;
      end

      let compare_packed : packed -> packed -> int = Poly.compare

      module Map = Stdlib.Map.Make (struct
          type t = packed

          let compare = compare_packed
        end)

      let of_identifier : type a. a Type.basic Identifier.t -> a Type.basic t =
        fun { ident; type_ = Basic type_ } ->
        match type_ with
        | Kind -> Kind
        | Mode -> Mode (Mode.of_mode_identifier ident)
        | Modality -> Modality (Modality.of_mode_identifier ident)
        | Alloc -> Alloc
      ;;

      let of_value : type a. a Type.basic Value.t -> a Type.basic t = function
        | Identifier ident -> of_identifier ident
        | Kind_product _ -> Kind
        | Kind_mod _ -> Kind
        | Tuple _ -> .
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
      | Kind_product kinds -> Kind_product (List.map kinds ~f:untype)
      | Kind_mod (kind, mods) -> Kind_mod (untype kind, List.map mods ~f:untype)
      | Tuple tp -> Tuple (untype_tuple tp)

    and untype_tuple : type a. a tuple -> Untyped.Value.t list = function
      | [] -> []
      | hd :: tl -> untype hd :: untype_tuple tl
    ;;

    let compare t1 t2 = Untyped.Value.compare (untype t1) (untype t2)

    let rec type_ : type a. a t -> a Type.t = function
      | Identifier ident -> ident.type_
      | Kind_mod _ -> Type.kind
      | Kind_product _ -> Type.kind
      | Tuple tp -> Tuple (type_tuple tp)

    and type_tuple : type a. a tuple -> a Type.tuple = function
      | [] -> []
      | hd :: tl -> type_ hd :: type_tuple tl
    ;;

    let is_default : type a. a Type.basic t -> bool =
      fun value ->
      match type_ value, value with
      | Basic Kind, Identifier { ident = "value" | "value_or_null"; _ } -> true
      | Basic Kind, _ -> false
      | Basic Mode, Identifier ident ->
        (match Axis.Sub_axis.of_identifier ident with
         | Mode Unrecognized -> false
         | Mode (Known axis) -> String.equal ident.ident (Axis.Sub_axis.Mode.default axis))
      | Basic Modality, Identifier ident ->
        (match Axis.Sub_axis.of_identifier ident with
         | Modality Unrecognized -> false
         | Modality (Known axis) ->
           String.equal ident.ident (Axis.Sub_axis.Modality.default axis))
      | Basic Alloc, Identifier { ident; _ } -> String.equal ident "heap"
      | Basic _, Tuple _ -> .
      | Tuple _, _ -> .
    ;;

    let rec to_node : type a. a Type.basic t -> loc:location -> a Type.basic Node.t =
      fun t ~loc ->
      match t with
      | Identifier { ident; type_ } ->
        (match type_ with
         | Basic Mode -> Mode { txt = Mode ident; loc }
         | Basic Modality -> Modality { txt = Modality ident; loc }
         | Basic Kind ->
           Jkind_annotation { pjkind_desc = Abbreviation ident; pjkind_loc = loc }
         | Basic Alloc -> Alloc
         | Tuple _ -> .)
      | Kind_product kinds ->
        let kinds =
          List.map kinds ~f:(fun kind ->
            let (Jkind_annotation kind) = to_node ~loc kind in
            kind)
        in
        Jkind_annotation { pjkind_desc = Product kinds; pjkind_loc = loc }
      | Kind_mod (kind, mods) ->
        let (Jkind_annotation kind) = to_node ~loc kind in
        let mods =
          List.sort_uniq mods ~cmp:compare
          |> List.map ~f:(fun (Identifier { ident; type_ = Basic Modality }) ->
            { txt = Mode ident; loc })
        in
        Jkind_annotation { pjkind_desc = Mod (kind, mods); pjkind_loc = loc }
      | Tuple _ -> .
    ;;
  end

  module Pattern = struct
    include Pattern

    let rec untype : type a. a t -> Untyped.Pattern.t = function
      | Identifier { ident; type_ = _ } -> Identifier { ident }
      | Tuple tp -> Tuple (untype_tuple tp)

    and untype_tuple : type a. a tuple -> Untyped.Pattern.t list = function
      | [] -> []
      | hd :: tl -> untype hd :: untype_tuple tl
    ;;

    let rec type_check
      : type a. Untyped.Pattern.t -> expected:a Type.t -> (a t, Type_error.t) result
      =
      let open Result.Let_syntax in
      fun untyped ~expected ->
        match untyped, expected with
        | Identifier { ident }, type_ -> Ok (Identifier { ident; type_ })
        | Tuple untyped_tp, Tuple type_tp ->
          let* vec =
            match Vec.of_list_with_length untyped_tp type_tp with
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
        | (Tuple _ as node), (Basic _ as type_) ->
          Error
            (Type_mismatch
               { kind = "pattern"
               ; sexp_of_kind = Untyped.Pattern.sexp_of_t
               ; value = node
               ; expected_type = type_
               })

    and type_check_tuple
      : type a.
        (Untyped.Pattern.t, a) Vec.t
        -> expected:a Type.tuple
        -> (a tuple, Type_error.t) result
      =
      fun untyped ~expected ->
      let open Result.Let_syntax in
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

    let rec type_ : type a. a t -> a Type.t = function
      | Identifier { type_; _ } -> type_
      | Kind_mod _ -> Type.kind
      | Kind_product _ -> Type.kind
      | Tuple tp -> Tuple (type_tuple tp)

    and type_tuple : type a. a tuple -> a Type.tuple = function
      | [] -> []
      | hd :: tl -> type_ hd :: type_tuple tl
    ;;

    let rec untype : type a. a t -> Untyped.Expression.t = function
      | Identifier { ident; type_ = _ } -> Identifier { ident }
      | Kind_mod (kind, mods) -> Kind_mod (untype kind, List.map mods ~f:untype)
      | Kind_product kinds -> Kind_product (List.map kinds ~f:untype)
      | Tuple tp -> Tuple (untype_tuple tp)

    and untype_tuple : type a. a tuple -> Untyped.Expression.t list = function
      | [] -> []
      | hd :: tl -> untype hd :: untype_tuple tl
    ;;

    let type_mismatch value type_ =
      Error
        (Type_error.Type_mismatch
           { kind = "expression"
           ; sexp_of_kind = Untyped.Expression.sexp_of_t
           ; value
           ; expected_type = type_
           })
    ;;

    let rec type_check
      : type a. Untyped.Expression.t -> expected:a Type.t -> (a t, Type_error.t) result
      =
      let open Result.Let_syntax in
      fun untyped ~expected ->
        match untyped, expected with
        | Identifier { ident }, type_ -> Ok (Identifier { ident; type_ })
        | Kind_product kinds, Basic Kind ->
          let+ kinds =
            List.map kinds ~f:(type_check ~expected:(Basic Kind)) |> Result.all
          in
          Kind_product kinds
        | (Kind_product _ as value), type_ -> type_mismatch value type_
        | Kind_mod (kind, mods), Basic Kind ->
          let* kind = type_check kind ~expected:(Basic Kind) in
          let+ mods =
            List.map mods ~f:(type_check ~expected:(Basic Modality)) |> Result.all
          in
          Kind_mod (kind, mods)
        | (Kind_mod _ as value), type_ -> type_mismatch value type_
        | Tuple untyped_tp, Tuple type_tp ->
          let* vec =
            match Vec.of_list_with_length untyped_tp type_tp with
            | Ok vec -> Ok vec
            | Error Wrong_length ->
              Error
                (Type_error.Tuple_length_mismatch
                   { kind = "expression"
                   ; sexp_of_kind = Untyped.Expression.sexp_of_t
                   ; value = Tuple untyped_tp
                   ; expected_type = Tuple type_tp
                   })
          in
          let+ tuple = type_check_tuple vec ~expected:type_tp in
          Tuple tuple
        | (Tuple _ as value), (Basic _ as type_) -> type_mismatch value type_

    and type_check_tuple
      : type a.
        (Untyped.Expression.t, a) Vec.t
        -> expected:a Type.tuple
        -> (a tuple, Type_error.t) result
      =
      let open Result.Let_syntax in
      fun untyped ~expected ->
        match untyped, expected with
        | [], [] -> Ok []
        | untyped_hd :: untyped_tl, type_hd :: type_tl ->
          let* hd = type_check untyped_hd ~expected:type_hd in
          let+ tl = type_check_tuple untyped_tl ~expected:type_tl in
          hd :: tl
    ;;
  end

  module Env = struct
    include Env

    let initial : t =
      let open Identifier in
      let heap_alloc = { ident = "heap"; type_ = Type.alloc } in
      let stack_alloc = { ident = "stack"; type_ = Type.alloc } in
      let global_mode = { ident = "global"; type_ = Type.mode } in
      let local_mode = { ident = "local"; type_ = Type.mode } in
      let heap_alloc_mode = { ident = "heap_global"; type_ = Type.(tuple2 alloc mode) } in
      let stack_alloc_mode =
        { ident = "stack_local"; type_ = Type.(tuple2 alloc mode) }
      in
      [ Entry (heap_alloc, Identifier heap_alloc)
      ; Entry (stack_alloc, Identifier stack_alloc)
      ; Entry (heap_alloc_mode, Tuple [ Identifier heap_alloc; Identifier global_mode ])
      ; Entry (stack_alloc_mode, Tuple [ Identifier stack_alloc; Identifier local_mode ])
      ]
    ;;

    let find (type a) (t : t) (ident : a Identifier.t) =
      List.find_map t ~f:(fun (Entry (ident', value)) ->
        Option.map (Identifier.equal_witness ident ident') ~f:(fun Equal : a Value.t ->
          value))
    ;;

    let rec bind : type a. t -> a Pattern.t -> a Value.t -> t =
      fun env pat value ->
      match pat, value with
      | Identifier pat, value ->
        (* We always conflate modes and modalities for the portability and contention axes.
         These axes have the property that the legacy mode coincides with the top mode
         for comonadic axes and bottom mode for monadic axes[^0]. When this holds,
         ['a @ m -> 'b @ n] is equivalent to ['a @@ m -> 'b @@ n].

         More thoroughly: let [t @@ m] be a type whenever [t] is a type and [m] is a
         modality, such that [t @@ m] behaves like
         {[
           type t_atat_m = { inner : t @@ m } [@@unboxed]
         ]}
         i.e. is a zero-cost modality box around the type. Note that we define [t @@ m]
         even when [m] is a modality that does nothing; for example, [t @@ local] behaves
         just as [t] (since the [local] modality does nothing[^1]).

         Then, for all modes/modalities [m] and [n] on comonadic (resp. monadic) axes, if
         we let [ext_m] and [ext_n] be the top (resp. bottom) mode of the corresponding
         axes ([ext] as in "extremum"), then ['a @ m -> 'b @ n] is equivalent to
         ['a @@ m @ ext_m -> 'b @@ n @ ext_n]. For example, ['a @ local -> 'b @ global]
         is equivalent to ['a @@ local @ local -> 'b @@ global @ local], and (since
         ['a @@ local] is just ['a]) also ['a @ local -> 'b @@ global @ local].

         To make conflating a mode with its corresponding modality act in unsurprising
         ways, we want ['a @ m -> 'b @ n] to be equivalent to ['a @@ m -> 'b @@ n], which
         is implicitly ['a @@ m @ legacy_m -> 'b @@ n @ legacy_n]. This holds exactly
         when [ext_m = legacy_m] and [ext_n = legacy_n].

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
         v}

         The only axes for which the right two columns align are portability and
         contention. For this reason, we always conflate modes and modalities on these two
         axes, and never on the other axes.

         [^0] The "top" (resp. "bottom") mode of an axis is the mode which is a super-mode
         (resp. sub-mode) of all other modes on the axis.

         [^1] Each modality acts as meet (min) for comonadic axes and join (max) for
         monadic axes; e.g. the [global] modality acts as [fun m -> meet global m].
         This means the [local] modality acts as [fun m -> meet local m], but since
         [local] is top for the locality axis, [meet local m = m], so [local] just
         acts as [fun m -> m], i.e. does nothing.
        *)
        let entries : Entry.t list =
          match pat, value with
          | ( { type_ = Basic (Mode | Modality); ident = pat_ident }
            , Identifier
                { ident =
                    ( "portable"
                    | "nonportable"
                    | "contended"
                    | "shared"
                    | "uncontended"
                    | "stateless"
                    | "observing"
                    | "stateful"
                    | "immutable"
                    | "read"
                    | "read_write" ) as expr_ident
                ; type_ = _
                } ) ->
            [ Entry
                ( { ident = pat_ident; type_ = Basic Mode }
                , Identifier { ident = expr_ident; type_ = Basic Mode } )
            ; Entry
                ( { ident = pat_ident; type_ = Basic Modality }
                , Identifier { ident = expr_ident; type_ = Basic Modality } )
            ]
          | _ -> [ Entry (pat, value) ]
        in
        entries @ env
      | Tuple pat_tp, Tuple value_tp -> bind_tuple env pat_tp value_tp
      | Tuple _, _ -> .

    and bind_tuple : type a. t -> a Pattern.tuple -> a Value.tuple -> t =
      fun env pat_tp value_tp ->
      match pat_tp, value_tp with
      | [], [] -> env
      | pat_hd :: pat_tl, value_hd :: value_tl ->
        bind_tuple (bind env pat_hd value_hd) pat_tl value_tl
    ;;

    let eval env { txt = expr; loc } =
      let rec loop : type a. a Expression.t -> a Value.t = function
        | Identifier ident as expr ->
          (match find env ident with
           | Some value -> value
           | None ->
             let typ = Expression.type_ expr in
             (match typ with
              | Basic (Mode | Modality | Kind) -> Identifier ident
              | Tuple _ | Basic Alloc ->
                let hint =
                  match typ, ident.ident with
                  | Basic Alloc, "heap_global" -> Some "Did you mean [heap]?"
                  | Basic Alloc, "stack_local" -> Some "Did you mean [stack]?"
                  | Tuple [ Basic Alloc; Basic Mode ], "heap" ->
                    Some "Did you mean [heap_global]?"
                  | Tuple [ Basic Alloc; Basic Mode ], "stack" ->
                    Some "Did you mean [stack_local]?"
                  | _ ->
                    (match
                       List.find_map env ~f:(fun (Entry (ident', _)) ->
                         if String.equal ident'.ident ident.ident
                         then Some (Type.sexp_of_t ident'.type_)
                         else None)
                     with
                     | None -> None
                     | Some type_ ->
                       Some
                         (Printf.sprintf
                            "There is a template identifier [%s] in scope with type [%s]."
                            ident.ident
                            (Sexp.to_string type_)))
                in
                let hint_string =
                  match hint with
                  | None -> ""
                  | Some hint -> "\nHint: " ^ hint
                in
                Location.raise_errorf
                  ~loc
                  "Unbound template identifier [%s] of type [%s].%s"
                  ident.ident
                  (Type.sexp_of_t typ |> Sexp.to_string_hum)
                  hint_string))
        | Kind_product kinds -> Kind_product (List.map kinds ~f:loop)
        | Kind_mod (kind, mods) ->
          Kind_mod (loop kind, List.map mods ~f:loop |> List.sort_uniq ~cmp:Value.compare)
        | Tuple tp -> Tuple (loop_tuple tp)
      and loop_tuple : type a. a Expression.tuple -> a Value.tuple = function
        | [] -> []
        | hd :: tl -> loop hd :: loop_tuple tl
      in
      loop expr
    ;;
  end
end

module Type_error = struct
  include Type_error

  let sexp_of_t : t -> Sexp.t = function
    | Type_mismatch { kind; sexp_of_kind; value; expected_type = type_ } ->
      List
        [ Atom "Type mismatch"
        ; List [ Atom "kind"; Atom kind ]
        ; List [ Atom "value"; sexp_of_kind value ]
        ; List [ Atom "expected type"; Typed.Type.sexp_of_t type_ ]
        ]
    | Tuple_length_mismatch { kind; sexp_of_kind; value; expected_type = type_ } ->
      List
        [ Atom "Tuple length mismatch"
        ; List [ Atom "kind"; Atom kind ]
        ; List [ Atom "value"; sexp_of_kind value ]
        ; List [ Atom "expected type"; Typed.Type.sexp_of_t type_ ]
        ]
  ;;

  let lift_to_error_result = function
    | Ok _ as ok -> ok
    | Error type_error -> Error (sexp_of_t type_error)
  ;;
end
