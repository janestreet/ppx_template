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
    | Tuple2 (t1, t2) -> List [ Atom "Tuple"; sexp_of_t t1; sexp_of_t t2 ]
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
       | 0 -> String.Set.compare mods1 mods2
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
        ; String.Set.to_list mods |> sexp_of_list sexp_of_string
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
        String.Set.to_list mods |> List.map ~f:(fun mode -> { txt = Mode mode; loc })
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
       | 0 -> String.Set.compare mods1 mods2
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
        ; String.Set.to_list mods |> sexp_of_list sexp_of_string
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

let cast : type a b. a Value.t -> b Type.t -> b Value.t =
  fun value as_ ->
  match value, as_ with
  | Identifier { ident; type_ = _ }, Basic _ -> Identifier { ident; type_ = as_ }
  | value, as_ ->
    failwith
      (List
         [ Atom
             "Internal [ppx_template] error: do not know how to cast non-identifier \
              values to other types."
         ; List [ Atom "value"; Value.sexp_of_t value ]
         ; List [ Atom "type"; Type.sexp_of_t as_ ]
         ]
       |> Sexp.to_string_hum)
;;

module Env = struct
  include Env

  let empty = []

  let find (type a) (t : t) (ident : a Identifier.t) =
    List.find_map t ~f:(fun (Entry (ident', value)) ->
      Option.map (Identifier.equal_witness ident ident') ~f:(fun Equal : a Value.t ->
        value))
  ;;

  let conflate_one t (P { new_identifier; existing_identifier } : Conflation.packed) =
    match find t existing_identifier with
    | None -> t
    | Some value ->
      let value' = cast value new_identifier.type_ in
      Entry (new_identifier, value') :: t
  ;;

  let conflate t conflations = List.fold_left conflations ~init:t ~f:conflate_one

  let rec bind : type a. t -> a Pattern.t -> a Value.t -> t =
    fun env pat value ->
    match pat, value with
    | Identifier pat, value -> Entry (pat, value) :: env
    | Tuple2 (pat1, pat2), Tuple2 (value1, value2) ->
      bind (bind env pat1 value1) pat2 value2
  ;;

  let rec eval : type a. t -> a Expression.t -> a Value.t =
    fun env expr ->
    match expr with
    | Identifier ident ->
      (match find env ident with
       | Some value -> value
       | None ->
         (match Expression.type_ expr with
          | Basic _ -> Identifier ident
          | Tuple2 _ ->
            Location.raise_errorf "Unbound template identifier %s." ident.ident))
    | Tuple2 (expr1, expr2) -> Tuple2 (eval env expr1, eval env expr2)
    | Kind_product kinds -> Kind_product (List.map kinds ~f:(eval env))
    | Kind_mod (kind, mods) -> Kind_mod (eval env kind, mods)
  ;;
end
