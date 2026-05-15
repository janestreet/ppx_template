open! Stdppx
open! Import
open Language
open Typed
open Result.Let_syntax

module Entry = struct
  type 'a t =
    { ident : 'a Identifier.t
    ; preserve_atoms : 'a Value.t
    (** A non-union-containing "alias" of this name. e.g. in
        [[@kind_set ks = value_with_imm]], the ident [value_with_imm] *)
    ; expand_atoms_bound_to_sets : 'a Value.t Nonempty_list.t
    (** The full set. e.g. in [[@kind_set ks = value_with_imm]], the list of idents
        [value], [immediate], [immediate64]. For names that are not obviously bound to
        sets, this is just the singleton set containing [preserve_atoms] *)
    ; namespace : 'a Axis.Namespace.t
    }

  type packed = P : _ t -> packed
end

module Entry_set : sig
  type t

  val empty : t
  val add : t -> Entry.packed -> t
  val add_all : t -> Entry.packed list -> t
  val find : t -> 'a Identifier.t -> 'a Entry.t option
  val find_by_name : t -> string -> Entry.packed list
end = struct
  module String_map = Map.Make (String)

  module Packed_type_map = Map.Make (struct
      type t = Type.packed

      let compare = Poly.compare
    end)

  type t = Entry.packed Packed_type_map.t String_map.t

  let empty : t = String_map.empty

  let add (t : t) (Entry.P entry) =
    String_map.update
      entry.ident.ident
      (fun packed_type_map ->
        Some
          (Option.value packed_type_map ~default:Packed_type_map.empty
           |> Packed_type_map.add (Type.P entry.ident.type_) (Entry.P entry)))
      t
  ;;

  let add_all (t : t) list = List.fold_left list ~init:t ~f:add

  let find (type a) (t : t) (id : a Identifier.t) : a Entry.t option =
    match String_map.find_opt id.ident t with
    | None -> None
    | Some packed_type_map ->
      (match Packed_type_map.find_opt (Type.P id.type_) packed_type_map with
       | None -> None
       | Some (Entry.P entry) ->
         (match Identifier.equal_witness id entry.ident with
          | None -> None
          | Some Equal -> Some entry))
  ;;

  let find_by_name (t : t) name : Entry.packed list =
    match String_map.find_opt name t with
    | None -> []
    | Some packed_type_map ->
      Packed_type_map.fold (fun _ entry acc -> entry :: acc) packed_type_map []
  ;;
end

type t = Entry_set.t

let value_entry ident value ~is_set : Entry.packed =
  P
    { ident
    ; preserve_atoms = value
    ; expand_atoms_bound_to_sets = [ value ]
    ; namespace = Axis.Namespace.of_value ~is_set value
    }
;;

let id_entry ident = value_entry ident (Identifier ident) ~is_set:true
let tuple_entry ident tuple = value_entry ident (Tuple tuple) ~is_set:false

let set_entry ident (values : _ Nonempty_list.t) : Entry.packed =
  let namespace =
    (* We make the simplifying assumption that all values in the set have have the same
       namespace *)
    Axis.Namespace.of_value ~is_set:true (Nonempty_list.hd values)
  in
  P
    { ident
    ; preserve_atoms = Identifier ident
    ; expand_atoms_bound_to_sets = values
    ; namespace
    }
;;

let initial : t =
  let open struct
    type 'a _identifier = 'a Identifier.t =
      { type_ : 'a Type.t
      ; ident : string
      }
  end in
  let alloc_mode_entries : Entry.packed list =
    let heap_alloc = { ident = "heap"; type_ = Type.alloc } in
    let stack_alloc = { ident = "stack"; type_ = Type.alloc } in
    let global_mode = { ident = "global"; type_ = Type.mode } in
    let local_mode = { ident = "local"; type_ = Type.mode } in
    let heap_alloc_mode = { ident = "heap_global"; type_ = Type.(tuple2 alloc mode) } in
    let stack_alloc_mode = { ident = "stack_local"; type_ = Type.(tuple2 alloc mode) } in
    [ id_entry heap_alloc
    ; id_entry stack_alloc
    ; tuple_entry heap_alloc_mode [ Identifier heap_alloc; Identifier global_mode ]
    ; tuple_entry stack_alloc_mode [ Identifier stack_alloc; Identifier local_mode ]
    ]
  in
  let synchro_mode_entries : Entry.packed list =
    let unsync = { ident = "unsync"; type_ = Type.synchro } in
    let sync = { ident = "sync"; type_ = Type.synchro } in
    let uncontended = { ident = "uncontended"; type_ = Type.mode } in
    let shared = { ident = "shared"; type_ = Type.mode } in
    let contended = { ident = "contended"; type_ = Type.mode } in
    let unsync_uncontended =
      { ident = "unsync_uncontended"; type_ = Type.(tuple2 synchro mode) }
    in
    let sync_shared = { ident = "sync_shared"; type_ = Type.(tuple2 synchro mode) } in
    let sync_contended =
      { ident = "sync_contended"; type_ = Type.(tuple2 synchro mode) }
    in
    [ id_entry unsync
    ; id_entry sync
    ; tuple_entry unsync_uncontended [ Identifier unsync; Identifier uncontended ]
    ; tuple_entry sync_shared [ Identifier sync; Identifier shared ]
    ; tuple_entry sync_contended [ Identifier sync; Identifier contended ]
    ]
  in
  let kind_set_entries : Entry.packed list =
    let kind_ident k = { ident = k; type_ = Type.kind } in
    let kind k : _ Value.t = Identifier (kind_ident k) in
    let base_non_value : _ Nonempty_list.t =
      [ kind "bits64"; kind "bits32"; kind "word"; kind "float64"; kind "float32" ]
    in
    (* [value] is last so that, when templating record or variant types, the fields and
       constructors for the untemplated type are the ones in scope. *)
    let value_with_imm : _ Nonempty_list.t =
      [ kind "immediate"; kind "immediate64"; kind "value" ]
    in
    let value_or_null_with_imm : _ Nonempty_list.t =
      [ kind "immediate"; kind "immediate64"; kind "value_or_null" ]
    in
    let kind_set_ident = kind_ident in
    [ set_entry (kind_set_ident "base_non_value") base_non_value
    ; set_entry (kind_set_ident "value_with_imm") value_with_imm
    ; set_entry (kind_set_ident "value_or_null_with_imm") value_or_null_with_imm
    ; set_entry
        (kind_set_ident "base")
        (Nonempty_list.concat [ base_non_value; [ kind "value" ] ])
    ; set_entry
        (kind_set_ident "base_with_imm")
        (Nonempty_list.concat [ base_non_value; value_with_imm ])
    ; set_entry
        (kind_set_ident "base_or_null")
        (Nonempty_list.concat [ base_non_value; [ kind "value_or_null" ] ])
    ; set_entry
        (kind_set_ident "base_or_null_with_imm")
        (Nonempty_list.concat [ base_non_value; value_or_null_with_imm ])
    ]
  in
  Entry_set.add_all
    Entry_set.empty
    (alloc_mode_entries @ synchro_mode_entries @ kind_set_entries)
;;

let find (type a) (t : t) (ident : a Identifier.t) =
  Entry_set.find t ident
  |> Option.map ~f:(fun (entry : a Entry.t) -> entry.preserve_atoms)
;;

let find_expanding_sets (type a) (t : t) (ident : a Identifier.t) =
  Entry_set.find t ident
  |> Option.map ~f:(fun (entry : a Entry.t) -> entry.expand_atoms_bound_to_sets)
;;

module Eval_result = struct
  type (_, _) t =
    | Singleton : 'a -> ('a, [ `one ]) t
    | Set : 'a Nonempty_list.t -> ('a, [ `many ]) t

  let map_res : type a b r e. (a, r) t -> f:(a -> (b, e) result) -> ((b, r) t, e) result =
    fun t ~f ->
    match t with
    | Singleton value ->
      let+ value = f value in
      Singleton value
    | Set value ->
      let+ value = Nonempty_list.Or_first_error.map value ~f in
      Set value
  ;;

  let map : type a b r. (a, r) t -> f:(a -> b) -> (b, r) t =
    fun t ~f ->
    match t with
    | Singleton value -> Singleton (f value)
    | Set values -> Set (Nonempty_list.map values ~f)
  ;;

  let bind_res
    : type a b r e. (a, r) t -> f:(a -> ((b, r) t, e) result) -> ((b, r) t, e) result
    =
    fun t ~f ->
    match t with
    | Singleton value -> f value
    | Set values ->
      let+ values =
        Nonempty_list.Or_first_error.map values ~f:(fun value ->
          let+ (Set values) = f value in
          values)
      in
      Set (Nonempty_list.concat values)
  ;;

  let product : type a r. (a, r) t Nonempty_list.t -> (a Nonempty_list.t, r) t =
    fun (hd :: tl) ->
    match hd with
    | Singleton hd ->
      let tl = List.map tl ~f:(fun (Singleton tl) -> tl) in
      Singleton (hd :: tl)
    | Set hd ->
      let tl = List.map tl ~f:(fun (Set tl) -> tl) in
      Set (Nonempty_list.product (hd :: tl))
  ;;

  module Witness = struct
    type _ size =
      | One : [ `one ] size
      | Many : [ `many ] size

    type (_, _, _) size_reason =
      | Simple : (Expression.singleton, [ `singleton_lookup ], [ `one ]) size_reason
      | Expr_has_unions : (Expression.set, _, [ `many ]) size_reason
      | Lookup_is_expanding_identifiers : (_, [ `set_lookup ], [ `many ]) size_reason

    type _ lookup =
      | Preserve_atoms : [ `singleton_lookup ] lookup
      | Expand_atoms_bound_to_sets : [ `set_lookup ] lookup

    type ('s, 'r) t =
      | Eval_witness :
          { is_set : (unit, 's) Expression.allow_set
          ; lookup : 'l lookup
          ; result_size : 'r size
          ; because : ('s, 'l, 'r) size_reason
          }
          -> ('s, 'r) t

    let singleton =
      Eval_witness
        { is_set = Singleton_only { why_no_set = () }
        ; lookup = Preserve_atoms
        ; result_size = One
        ; because = Simple
        }
    ;;

    let explicit_set ~lookup =
      Eval_witness
        { is_set = Set_or_singleton
        ; lookup
        ; result_size = Many
        ; because = Expr_has_unions
        }
    ;;

    let expand_atoms ~is_set =
      Eval_witness
        { is_set
        ; lookup = Expand_atoms_bound_to_sets
        ; result_size = Many
        ; because = Lookup_is_expanding_identifiers
        }
    ;;
  end
end

include struct
  open Ppx_helpers.Ox

  let rec interpret_kind
    : loc:location -> Type.kind Value.t -> (Kind.t, Syntax_error.t) result
    =
    fun ~loc -> function
    | Identifier { ident; type_ = _ } ->
      (try Ok (Kind.of_ident_exn ~ident) with
       | _ -> Error (Syntax_error.createf ~loc "Unrecognized kind identifier: %s" ident))
    | Kind_product _ ->
      Error
        (Syntax_error.createf
           ~loc
           "Interpreting kind products in coercions is not supported")
    | Kind_mod (kind, mods) ->
      let* kind = interpret_kind ~loc kind in
      let+ mods =
        Nonempty_list.Or_first_error.map mods ~f:(fun (Identifier { ident; type_ = _ }) ->
          try Ok (Jkind_mod.of_string ident) with
          | _ -> Error (Syntax_error.createf ~loc "Unrecognized crossing: %s" ident))
        >>| Nonempty_list.to_list
      in
      Kind.apply_mods kind (Jkind_modifiers.of_jkind_mods mods)
  ;;
end

let rec eval_general
  : type a s r.
    t
    -> loc:location
    -> (s, r) Eval_result.Witness.t
    -> (a, s) Expression.t
    -> ((a Value.t, r) Eval_result.t, Syntax_error.t) result
  =
  fun env ~loc eval_witness expr ->
  let (Eval_witness { is_set; lookup; result_size; because = result_size_reason }) =
    eval_witness
  in
  match expr with
  | Kind_product kinds ->
    kinds
    |> Nonempty_list.Or_first_error.map ~f:(eval_general env ~loc eval_witness)
    >>| Eval_result.product
    >>| Eval_result.map ~f:(fun kinds : _ Value.t -> Kind_product kinds)
  | Kind_mod (kind, mods) ->
    let* kind = eval_general env ~loc eval_witness kind in
    let+ (Singleton mods) =
      Nonempty_list.Or_first_error.map
        ~f:(eval_general env ~loc Eval_result.Witness.singleton)
        mods
      >>| Eval_result.product
    in
    let mods = Nonempty_list.sort_uniq ~cmp:Value.compare mods in
    Eval_result.map kind ~f:(fun kind : _ Value.t -> Kind_mod (kind, mods))
  | Kind_coercion (kind, coerce_to) ->
    let* kind = eval_general env ~loc eval_witness kind in
    let* (Set coerce_to) =
      eval_general
        env
        ~loc
        (Eval_result.Witness.explicit_set ~lookup:Expand_atoms_bound_to_sets)
        coerce_to
    in
    let* coerce_to =
      coerce_to
      |> Nonempty_list.Or_first_error.map ~f:(fun kind ->
        let+ interpreted = interpret_kind ~loc kind in
        kind, interpreted)
      >>| Nonempty_list.to_list
    in
    Eval_result.map_res kind ~f:(fun kind ->
      match interpret_kind ~loc kind with
      | Error _ ->
        (* Permit invalid kinds on the LHS of the coercion *)
        Ok kind
      | Ok interpreted ->
        (match
           List.filter coerce_to ~f:(fun (_, coerce_to) ->
             Ppx_helpers.Ox.Kind.is_subkind interpreted ~of_:coerce_to)
         with
         | [] -> Ok kind
         | hd :: tl as coerced_to ->
           let+ () =
             (* If there is more than one coercion result, we want to assert that they are
                all comparable, then take the most specific. If the coercions are not
                comparable, we error. *)
             List.Or_first_error.iter coerced_to ~f:(fun (val1, k1) ->
               (* We do double the work here, but that's fine, these lists are always
                  small *)
               List.Or_first_error.iter coerced_to ~f:(fun (val2, k2) ->
                 if not
                      Ppx_helpers.Ox.Kind.(is_subkind k1 ~of_:k2 || is_subkind k2 ~of_:k1)
                 then (
                   let to_string kind =
                     kind |> Value.sexp_of_t |> Sexplib0.Sexp.to_string_hum
                   in
                   Error
                     (Syntax_error.createf
                        ~loc
                        "Kind is a subkind of two incomparable coercion options:\n\
                        \  %s\n\
                        \  is subkind of %s\n\
                        \  and           %s\n\
                        \  But neither is a subkind of the other\n"
                        (to_string kind)
                        (to_string val1)
                        (to_string val2)))
                 else Ok ()))
           in
           (* Take the most specific kind *)
           let res, _ =
             List.fold_left tl ~init:hd ~f:(fun ((_, k1) as min) ((_, k2) as el) ->
               if Ppx_helpers.Ox.Kind.is_subkind k1 ~of_:k2 then min else el)
           in
           res))
  | Tuple tp ->
    let+ tp = eval_tuple env ~loc tp in
    let value : _ Value.t = Tuple tp in
    (match result_size with
     | One -> (Singleton value : (_, r) Eval_result.t)
     | Many -> Set [ value ])
  | Union exprs ->
    let Set_or_singleton, Many, _ = is_set, result_size, result_size_reason in
    Eval_result.bind_res (Set exprs) ~f:(fun expr ->
      expr
      |> Expression.to_set
      |> eval_general env ~loc (Eval_result.Witness.explicit_set ~lookup))
  | Identifier ident as expr ->
    let found : (_, r) Eval_result.t option =
      match lookup, result_size, result_size_reason with
      | Preserve_atoms, One, _ ->
        Option.map (find env ident) ~f:(fun value -> Eval_result.Singleton value)
      | Preserve_atoms, Many, _ ->
        Option.map (find env ident) ~f:(fun value -> Eval_result.Set [ value ])
      | Expand_atoms_bound_to_sets, Many, _ ->
        Option.map (find_expanding_sets env ident) ~f:(fun values ->
          Eval_result.Set values)
      | _ -> .
    in
    (match found with
     | Some value -> Ok value
     | None ->
       let typ = Expression.type_ expr in
       (match typ with
        | Non_tuple (Mode | Modality | Kind) ->
          (* We assume the identifier is a built-in and let the compiler provide an error
             message if not. *)
          Ok
            (match result_size with
             | One -> Singleton (Identifier ident)
             | Many -> Set [ Identifier ident ])
        | Tuple _ | Non_tuple Alloc | Non_tuple Synchro ->
          let hint =
            match typ, ident.ident with
            (* Alloc typos *)
            | Non_tuple Alloc, "heap_global" -> Some "Did you mean [heap]?"
            | Non_tuple Alloc, "stack_local" -> Some "Did you mean [stack]?"
            | Tuple [ Non_tuple Alloc; Non_tuple Mode ], "heap" ->
              Some "Did you mean [heap_global]?"
            | Tuple [ Non_tuple Alloc; Non_tuple Mode ], "stack" ->
              Some "Did you mean [stack_local]?"
            (* Synchro typos *)
            | Non_tuple Synchro, "unsync_uncontended" -> Some "Did you mean [unsync]?"
            | Non_tuple Synchro, ("sync_shared" | "sync_contended") ->
              Some "Did you mean [sync]?"
            | Tuple [ Non_tuple Synchro; Non_tuple Mode ], "unsync" ->
              Some "Did you mean [unsync_uncontended]?"
            | Tuple [ Non_tuple Synchro; Non_tuple Mode ], "sync" ->
              Some "Did you mean [sync_shared] or [sync_contended]?"
            (* Type mismatch *)
            | _ ->
              (match Entry_set.find_by_name env ident.ident with
               | [] -> None
               | Entry.P entry :: _ ->
                 Some
                   (Printf.sprintf
                      "There is a template identifier [%s] in scope with type [%s]."
                      ident.ident
                      (Sexp.to_string (Type.sexp_of_t entry.ident.type_))))
          in
          let hint_string =
            match hint with
            | None -> ""
            | Some hint -> "\nHint: " ^ hint
          in
          Error
            (Syntax_error.createf
               ~loc
               "Unbound template identifier [%s] of type [%a].%s"
               ident.ident
               Sexplib0.Sexp.pp_hum
               (Type.sexp_of_t typ)
               hint_string)))

and eval_tuple
  : type a.
    t -> loc:location -> a Expression.tuple -> (a Value.tuple, Syntax_error.t) result
  =
  fun env ~loc tuple ->
  match tuple with
  | [] -> Ok []
  | hd :: tl ->
    let* (Singleton hd) = eval_general env ~loc Eval_result.Witness.singleton hd in
    let+ tl = eval_tuple env ~loc tl in
    (hd :: tl : _ Value.tuple)
;;

let eval_singleton
  : type a.
    t
    -> (a, Expression.singleton) Expression.t Loc.t
    -> (a Value.t, Syntax_error.t) result
  =
  fun env { txt = expr; loc } ->
  let+ (Singleton res) = eval_general env ~loc Eval_result.Witness.singleton expr in
  res
;;

let eval
  : type a.
    t
    -> Attribute_handler.lookup
    -> (a, Expression.set) Expression.t Loc.t
    -> (a Value.t Nonempty_list.t, Syntax_error.t) result
  =
  fun env lookup { txt = expr; loc } ->
  let[@inline] eval_lookup lookup =
    let+ (Set values) =
      eval_general env ~loc (Eval_result.Witness.explicit_set ~lookup) expr
    in
    values
  in
  match lookup with
  | Preserve_atoms -> eval_lookup Preserve_atoms
  | Expand_atoms_bound_to_sets -> eval_lookup Expand_atoms_bound_to_sets
;;

(* We prohibit shadowing a variable that is already bound to values for a different
   sub-axis. Shadowing a variable in the same sub-axis is permitted. *)
let check_ident_shadowing env ~loc ~is_set (pat : _ Identifier.t) value =
  let namespace = Axis.Namespace.of_value ~is_set value in
  match
    List.filter (Entry_set.find_by_name env pat.ident) ~f:(fun (Entry.P entry) ->
      not (Axis.Namespace.same_namespace entry.namespace namespace))
  with
  | [] ->
    (* this identifier is not shadowing *)
    Ok ()
  | _ :: tail as entries ->
    Error
      (Syntax_error.createf
         ~loc
         "shadowing variables from a different namespace is prohibited%s\n\
          attempting to bind\n\
         \  identifier '%s'\n\
         \  in namespace '%a'\n\
          but it is already bound\n\
          %a"
         (match tail with
          | [] -> ""
          | _ :: _ -> "\n(modulo mode/modality conflation)")
         pat.ident
         Sexplib0.Sexp.pp
         (Axis.Namespace.sexp_of_t namespace)
         (fun fmt entries ->
           List.iteri
             entries
             ~f:
               (fun
                 i
                 (Entry.P
                   { ident = _
                   ; preserve_atoms = old_value
                   ; expand_atoms_bound_to_sets = set_value
                   ; namespace = old_namespace
                   })
               ->
               Format.fprintf
                 fmt
                 "%s  in namespace '%a'\n\
                 \  to the value '%a'\n\
                 \  and when fully expanded '%a'"
                 (if i = 0 then "" else "\nand\n")
                 Sexplib0.Sexp.pp
                 (Axis.Namespace.sexp_of_t old_namespace)
                 Sexplib0.Sexp.pp
                 (Value.sexp_of_t old_value)
                 Sexplib0.Sexp.pp
                 (Nonempty_list.sexp_of_t Value.sexp_of_t set_value)))
         entries)
;;

(* we only cast modes and modalities *)
type 'a castable_witness =
  | Mode : Type.mode castable_witness
  | Modality : Type.modality castable_witness

let cast_entry
  (type a b)
  (entry : a Entry.t)
  (type_ : b Type.non_tuple Type.t)
  (castable : a castable_witness)
  ~is_set
  : Entry.packed
  =
  let { ident; preserve_atoms; expand_atoms_bound_to_sets; namespace = _ } : _ Entry.t =
    entry
  in
  let cast_ident (ident : a Identifier.t) : b Type.non_tuple Identifier.t =
    { type_; ident = ident.ident }
  in
  let cast_value (value : a Value.t) : b Type.non_tuple Value.t =
    match value, castable with
    | Identifier ident, _ -> Identifier (cast_ident ident)
    | _, _ -> .
  in
  let preserve_atoms = cast_value preserve_atoms in
  P
    { ident = cast_ident ident
    ; preserve_atoms
    ; expand_atoms_bound_to_sets =
        Nonempty_list.map expand_atoms_bound_to_sets ~f:cast_value
    ; namespace = Axis.Namespace.of_value ~is_set preserve_atoms
    }
;;

let rec bind
  : type a.
    t
    -> loc:location
    -> is_set:bool
    -> a Pattern.t
    -> a Value.t
    -> (t, Syntax_error.t) result
  =
  fun env ~loc ~is_set pat value ->
  match pat, value with
  | Wildcard, _ ->
    (* Don't put wildcard patterns [_] into the environment, just like OCaml. *)
    Ok env
  | Identifier pat, value ->
    let* () = check_ident_shadowing env ~loc ~is_set pat value in
    let+ (Set values) =
      value
      |> Value.as_expression
      |> eval_general
           env
           ~loc
           (Eval_result.Witness.expand_atoms ~is_set:(Singleton_only { why_no_set = () }))
    in
    let entry : _ Entry.t =
      { ident = pat
      ; preserve_atoms = value
      ; expand_atoms_bound_to_sets = values
      ; namespace = Axis.Namespace.of_value ~is_set value
      }
    in
    let entries : Entry.packed list =
      (* See comment about conflating axes at bottom of file. *)
      let (value :: _) = values in
      match value with
      | Identifier _ as value ->
        (match Axis.Sub_axis.of_value value with
         | Mode (Known (Mode (Portability | Contention | Statefulness | Visibility))) ->
           [ P entry; cast_entry entry Type.modality Mode ~is_set ]
         | Modality
             (Known (Modality (Portability | Contention | Statefulness | Visibility))) ->
           [ P entry; cast_entry entry Type.mode Modality ~is_set ]
         | _ -> [ P entry ])
      | _ -> [ P entry ]
    in
    Entry_set.add_all env entries
  | Tuple pat_tp, Tuple value_tp -> bind_tuple ~loc ~is_set env pat_tp value_tp
  | Tuple _, _ -> .

and bind_tuple
  : type a.
    t
    -> loc:location
    -> is_set:bool
    -> a Pattern.tuple
    -> a Value.tuple
    -> (t, Syntax_error.t) result
  =
  fun env ~loc ~is_set pat_tp value_tp ->
  match pat_tp, value_tp with
  | [], [] -> Ok env
  | pat_hd :: pat_tl, value_hd :: value_tl ->
    let* env = bind env ~loc ~is_set pat_hd value_hd in
    bind_tuple env ~loc ~is_set pat_tl value_tl
;;

let bind_set
  : type a.
    t
    -> loc:location
    -> a Type.non_tuple Pattern.t
    -> a Type.non_tuple Value.t Nonempty_list.t
    -> (t, Syntax_error.t) result
  =
  fun env ~loc pattern values ->
  match pattern with
  | Wildcard ->
    (* Don't put wildcard patterns [_] into the environment, just like OCaml. *)
    Ok env
  | Identifier ident ->
    let+ () =
      (* we sample one value from the set to check *)
      let (value :: _) = values in
      check_ident_shadowing env ~loc ~is_set:true ident value
    in
    Entry_set.add env (set_entry ident values)
;;

let fold_combinatorial_results list ~init ~f =
  List.Or_first_error.fold_left
    list
    ~init:(Nonempty_list.singleton init)
    ~f:(fun accs elt ->
      Nonempty_list.Or_first_error.concat_map accs ~f:(fun acc -> f acc elt))
;;

type 'mangle eval =
  | Eval :
      'a 'mangle.
      'a Pattern.t Loc.t * ('a Value.t * 'mangle Type.non_tuple Value.t) Nonempty_list.t
      -> 'mangle eval

type eval_set =
  | Eval_set :
      'mangle.
      'mangle Type.non_tuple Axis.t * Explicitness.t * 'mangle eval list
      -> eval_set

let eval_attributes
  t
  (poly_attributes : Attribute_handler.Poly.t Explicitness.With.t list)
  =
  List.Or_first_error.map
    poly_attributes
    ~f:(fun { explicitness; what = Poly (mangle_axis, bindings) } ->
      let+ evals =
        List.Or_first_error.map
          bindings
          ~f:(fun (P { binding = { pattern; expression; lookup }; selector }) ->
            let+ values = eval t lookup expression in
            let with_mangled =
              values
              |> Nonempty_list.stable_dedup ~cmp:Value.compare
              |> Nonempty_list.map ~f:(fun value ->
                value, Attribute_handler.Binding.Selector.select selector value)
            in
            Eval ({ txt = pattern; loc = expression.loc }, with_mangled))
      in
      Eval_set (mangle_axis, explicitness, evals))
;;

let bind_attributes original_env eval_sets =
  let open Explicitness.With in
  fold_combinatorial_results
    eval_sets
    ~init:(original_env, Axis.Map.empty)
    ~f:(fun (env, manglers) (Eval_set (mangle_axis, explicitness, evals)) ->
      let is_set = Axis.is_set mangle_axis in
      let+ axes =
        fold_combinatorial_results
          evals
          ~init:(env, [])
          ~f:(fun (env, values) (Eval (pattern, with_mangled)) ->
            Nonempty_list.Or_first_error.map with_mangled ~f:(fun (value, mangled) ->
              let+ env = bind env ~loc:pattern.loc ~is_set pattern.txt value in
              env, Value.Basic.P mangled :: values))
      in
      Nonempty_list.map axes ~f:(fun (env, values) ->
        ( env
        , Axis.Map.add (P mangle_axis) { explicitness; what = List.rev values } manglers )))
;;

let instantiate t poly_attributes =
  eval_attributes t poly_attributes >>= bind_attributes t
;;
