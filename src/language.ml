open! Stdppx
open! Import
include Language_intf.Definitions

module Type = struct
  include Type

  let equal_witness_basic : type a b. a basic -> b basic -> (a, b) Stdlib.Type.eq option =
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
    | Tuple2 (t1a, t1b), Tuple2 (t2a, t2b) ->
      (match equal_witness t1a t2a with
       | Some Equal ->
         (match equal_witness t1b t2b with
          | Some Equal -> Some Equal
          | None -> None)
       | None -> None)
    | (Basic _ | Tuple2 _), _ -> None
  ;;

  let rec sexp_of_t : type a. a t -> Sexp.t = function
    | Basic basic -> sexp_of_basic basic
    | Tuple2 (t1, t2) -> List [ sexp_of_t t1; Atom "@"; sexp_of_t t2 ]
  ;;

  module Map = Map.Make (struct
      type t = packed

      let compare = Poly.compare
    end)

  let kind = Basic Kind
  let mode = Basic Mode
  let modality = Basic Modality
  let alloc = Basic Alloc
  let tuple2 t1 t2 = Tuple2 (t1, t2)
end

module Identifier = struct
  include Identifier

  let compare : 'a t -> 'a t -> int = Poly.compare

  let equal_witness { type_; ident } t =
    match Type.equal_witness type_ t.type_ with
    | Some _ as eq when String.equal ident t.ident -> eq
    | _ -> None
  ;;

  let sexp_of_t { ident; type_ = _ } : Sexp.t = Atom ident
end

module Value = struct
  include Value

  let rec compare : type a. a t -> a t -> int =
    fun t1 t2 ->
    match t1, t2 with
    | Identifier ident1, Identifier ident2 -> Identifier.compare ident1 ident2
    | Kind_product kinds1, Kind_product kinds2 -> List.compare kinds1 kinds2 ~cmp:compare
    | Kind_mod (kind1, mods1), Kind_mod (kind2, mods2) ->
      (match compare kind1 kind2 with
       | 0 ->
         let mods1 = List.sort_uniq ~cmp:compare mods1 in
         let mods2 = List.sort_uniq ~cmp:compare mods2 in
         List.compare ~cmp:compare mods1 mods2
       | n -> n)
    | Tuple2 (t1a, t1b), Tuple2 (t2a, t2b) ->
      (match compare t1a t2a with
       | 0 -> compare t1b t2b
       | n -> n)
    | Identifier _, _ -> -1
    | _, Identifier _ -> 1
    | Kind_product _, _ -> -1
    | _, Kind_product _ -> 1
    | Kind_mod _, _ -> .
    | _, Kind_mod _ -> .
    | Tuple2 _, _ -> .
    | _, Tuple2 _ -> .
  ;;

  let rec sexp_of_t : type a. a t -> Sexp.t = function
    | Identifier ident -> Identifier.sexp_of_t ident
    | Kind_product kinds -> List [ Atom "Product"; sexp_of_list sexp_of_t kinds ]
    | Kind_mod (kind, mods) ->
      List
        [ Atom "Mod"
        ; sexp_of_t kind
        ; List.sort_uniq mods ~cmp:compare |> sexp_of_list sexp_of_t
        ]
    | Tuple2 (t1, t2) -> List [ Atom "Tuple"; sexp_of_t t1; sexp_of_t t2 ]
  ;;

  let rec type_ : type a. a t -> a Type.t = function
    | Identifier ident -> ident.type_
    | Kind_mod _ -> Type.kind
    | Kind_product _ -> Type.kind
    | Tuple2 (v1, v2) -> Tuple2 (type_ v1, type_ v2)
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
       | Basic Alloc -> Alloc)
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
  ;;
end

module Pattern = struct
  include Pattern

  let rec compare : type a. a t -> a t -> int =
    fun t1 t2 ->
    match t1, t2 with
    | Identifier ident1, Identifier ident2 -> Identifier.compare ident1 ident2
    | Tuple2 (t1a, t1b), Tuple2 (t2a, t2b) ->
      (match compare t1a t2a with
       | 0 -> compare t1b t2b
       | n -> n)
    | Identifier _, _ -> -1
    | _, Identifier _ -> 1
    | Tuple2 _, _ -> .
    | _, Tuple2 _ -> .
  ;;

  let rec sexp_of_t : type a. a t -> Sexp.t = function
    | Identifier ident -> Identifier.sexp_of_t ident
    | Tuple2 (t1, t2) -> List [ Atom "Tuple"; sexp_of_t t1; sexp_of_t t2 ]
  ;;
end

module Expression = struct
  include Expression

  let rec compare : type a. a t -> a t -> int =
    fun t1 t2 ->
    match t1, t2 with
    | Identifier ident1, Identifier ident2 -> Identifier.compare ident1 ident2
    | Kind_product kinds1, Kind_product kinds2 -> List.compare kinds1 kinds2 ~cmp:compare
    | Kind_mod (kind1, mods1), Kind_mod (kind2, mods2) ->
      (match compare kind1 kind2 with
       | 0 ->
         let mods1 = List.sort_uniq ~cmp:compare mods1 in
         let mods2 = List.sort_uniq ~cmp:compare mods2 in
         List.compare ~cmp:compare mods1 mods2
       | n -> n)
    | Tuple2 (t1a, t1b), Tuple2 (t2a, t2b) ->
      (match compare t1a t2a with
       | 0 -> compare t1b t2b
       | n -> n)
    | Identifier _, _ -> -1
    | _, Identifier _ -> 1
    | Kind_product _, _ -> -1
    | _, Kind_product _ -> 1
    | Kind_mod _, _ -> .
    | _, Kind_mod _ -> .
    | Tuple2 _, _ -> .
    | _, Tuple2 _ -> .
  ;;

  let rec sexp_of_t : type a. a t -> Sexp.t = function
    | Identifier ident -> Identifier.sexp_of_t ident
    | Kind_product kinds -> List (Atom "Product" :: List.map kinds ~f:sexp_of_t)
    | Kind_mod (kind, mods) ->
      List
        [ Atom "Mod"
        ; sexp_of_t kind
        ; List.sort_uniq mods ~cmp:compare |> sexp_of_list sexp_of_t
        ]
    | Tuple2 (t1, t2) -> List [ Atom "Tuple"; sexp_of_t t1; sexp_of_t t2 ]
  ;;

  let rec type_ : type a. a t -> a Type.t =
    fun t ->
    match t with
    | Identifier { type_; _ } -> type_
    | Kind_mod _ -> Type.kind
    | Kind_product _ -> Type.kind
    | Tuple2 (expr1, expr2) -> Tuple2 (type_ expr1, type_ expr2)
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
    let stack_alloc_mode = { ident = "stack_local"; type_ = Type.(tuple2 alloc mode) } in
    [ Entry (heap_alloc, Identifier heap_alloc)
    ; Entry (stack_alloc, Identifier stack_alloc)
    ; Entry (heap_alloc_mode, Tuple2 (Identifier heap_alloc, Identifier global_mode))
    ; Entry (stack_alloc_mode, Tuple2 (Identifier stack_alloc, Identifier local_mode))
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
                  ("portable" | "nonportable" | "contended" | "shared" | "uncontended") as
                  expr_ident
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
    | Tuple2 (pat1, pat2), Tuple2 (value1, value2) ->
      bind (bind env pat1 value1) pat2 value2
  ;;

  let eval env { txt = expr; loc } =
    let rec loop : type a. a Expression.t -> a Value.t =
      fun expr ->
      match expr with
      | Identifier ident ->
        (match find env ident with
         | Some value -> value
         | None ->
           let typ = Expression.type_ expr in
           (match typ with
            | Basic (Mode | Modality | Kind) -> Identifier ident
            | Tuple2 _ | Basic Alloc ->
              let hint =
                match typ, ident.ident with
                | Basic Alloc, "heap_global" -> Some "Did you mean [heap]?"
                | Basic Alloc, "stack_local" -> Some "Did you mean [stack]?"
                | Tuple2 (Basic Alloc, Basic Mode), "heap" ->
                  Some "Did you mean [heap_global]?"
                | Tuple2 (Basic Alloc, Basic Mode), "stack" ->
                  Some "Did you mean [stack_local]?"
                | _ -> None
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
      | Tuple2 (expr1, expr2) -> Tuple2 (loop expr1, loop expr2)
      | Kind_product kinds -> Kind_product (List.map kinds ~f:loop)
      | Kind_mod (kind, mods) ->
        Kind_mod (loop kind, List.map mods ~f:loop |> List.sort_uniq ~cmp:Value.compare)
    in
    loop expr
  ;;
end
