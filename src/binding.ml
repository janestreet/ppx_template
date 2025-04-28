open! Stdppx
open! Import
include Binding_intf.Definitions

module Kind = struct
  type identifier = Identifier.Kind.t
  type node = jkind_annotation

  type t =
    | Abbreviation of Identifier.Kind.t
    | Product of t list
    | Mod of t * String.Set.t

  let of_identifier id = Abbreviation id

  let rec compare t1 t2 =
    match t1, t2 with
    | Abbreviation id1, Abbreviation id2 -> Identifier.Kind.compare id1 id2
    | Product ts1, Product ts2 -> List.compare ~cmp:compare ts1 ts2
    | Mod (base1, modifiers1), Mod (base2, modifiers2) ->
      (match compare base1 base2 with
       | 0 -> String.Set.compare modifiers1 modifiers2
       | c -> c)
    (* abbrev < product < mod *)
    | Abbreviation _, Product _ | Abbreviation _, Mod _ | Product _, Mod _ -> -1
    | Mod _, Product _ | Mod _, Abbreviation _ | Product _, Abbreviation _ -> 1
  ;;

  let pattern () =
    let report_syntax_error ~loc =
      Location.raise_errorf
        ~loc
        "expected a kind abbreviation, product of kinds, or kind with a mod"
    in
    let rec of_expr = function
      | { pexp_desc = Pexp_ident { txt = Lident kind; _ }; _ } ->
        Abbreviation (Identifier.Kind.of_string kind)
      | [%expr [%e? lhs] & [%e? rhs]] ->
        let lhs = of_expr lhs in
        let rhs =
          match of_expr rhs with
          | (Abbreviation _ | Mod _) as rhs -> [ rhs ]
          | Product rhs -> rhs
        in
        Product (lhs :: rhs)
      | [%expr [%e? base] mod [%e? modifiers_exp]] as expr ->
        let base = of_expr base in
        let modifier_exps =
          match modifiers_exp with
          | { pexp_desc = Pexp_apply (modifiers_hd, modifiers_tl); _ } ->
            let modifiers_tl =
              List.map modifiers_tl ~f:(fun (label, modifier) ->
                match label with
                | Nolabel -> modifier
                | Labelled _ | Optional _ -> report_syntax_error ~loc:expr.pexp_loc)
            in
            modifiers_hd :: modifiers_tl
          | modifiers_hd -> [ modifiers_hd ]
        in
        let modifiers =
          List.map modifier_exps ~f:(function
            | { pexp_desc = Pexp_ident { txt = Lident modifier; _ }; _ } -> modifier
            | { pexp_loc = loc; _ } -> report_syntax_error ~loc)
        in
        Mod (base, String.Set.of_list modifiers)
      | { pexp_loc = loc; _ } -> report_syntax_error ~loc
    in
    Ast_pattern.of_func (fun (_ : Ast_pattern.context) (_ : location) expr k ->
      k (of_expr expr))
  ;;

  (* This is a bit fragile. It's easy to end up with identifiers bound to themselves, e.g.
     via {[
       let[@kind float64] id x = x
     ]}, which desugars to {[
       let[@kind float64 = float64] id x = x
     ]}. If we just naively looked up the abbreviation here, we'd end up in an infinite
     loop.

     So instead, we rely on [Env.lookup_and_set_all] to eagerly resolve all identifiers as
     soon as we see them, such that we should never need to recursively chase through
     abbreviations.

     Thus, the recursion here is only to handle nested products. *)
  let rec resolve t ~find_identifier =
    match t with
    | Abbreviation kind -> find_identifier kind |> Option.value ~default:t
    | Product ts -> Product (List.map ts ~f:(resolve ~find_identifier))
    | Mod (base, modifiers) ->
      (match resolve base ~find_identifier with
       | Mod (base, more_modifiers) ->
         Mod (base, String.Set.union modifiers more_modifiers)
       | (Abbreviation _ | Product _) as base -> Mod (base, modifiers))
  ;;

  let rec to_node ~loc t : jkind_annotation =
    { pjkind_desc =
        (match t with
         | Abbreviation kind -> Abbreviation (Identifier.Kind.to_string kind)
         | Product ts ->
           Product
             (List.mapi ts ~f:(fun i t ->
                to_node ~loc:{ loc with loc_ghost = loc.loc_ghost || i > 0 } t))
         | Mod (base, modifiers) ->
           Mod
             ( to_node ~loc base
             , modifiers
               |> String.Set.to_list
               |> List.map ~f:(fun x ->
                 { txt = Mode x; loc = { loc with loc_ghost = true } }) ))
    ; pjkind_loc = loc
    }
  ;;

  let identifier_of_strings strings =
    strings
    |> String.concat ~sep:"_"
    |> Printf.sprintf "'%s'"
    |> Identifier.Kind.of_string
  ;;

  let rec to_mangled_identifier = function
    | Abbreviation kind -> kind
    | Product ts -> List.map ts ~f:to_mangled_string |> identifier_of_strings
    | Mod (base, modifiers) ->
      to_mangled_string base :: "mod" :: String.Set.to_list modifiers
      |> identifier_of_strings

  and to_mangled_string t = t |> to_mangled_identifier |> Identifier.Kind.to_string
end

module Mode = struct
  type identifier = Identifier.Mode.t
  type node = mode
  type t = Identifier.Mode.t

  let of_identifier id = id
  let compare = Identifier.Mode.compare

  let pattern () =
    Ast_pattern.(pexp_ident (map1 (lident __) ~f:Identifier.Mode.of_string))
  ;;

  let resolve t ~find_identifier = find_identifier t |> Option.value ~default:t
  let to_node ~loc:(_ : location) t = Mode (Identifier.Mode.to_string t)
  let to_mangled_identifier t = t
end

module Modality = struct
  type identifier = Identifier.Modality.t
  type node = modality
  type t = Identifier.Modality.t

  let of_identifier id = id
  let compare = Identifier.Modality.compare

  let pattern () =
    Ast_pattern.(pexp_ident (map1 (lident __) ~f:Identifier.Modality.of_string))
  ;;

  let resolve t ~find_identifier = find_identifier t |> Option.value ~default:t
  let to_node ~loc:(_ : location) t = Modality (Identifier.Modality.to_string t)
  let to_mangled_identifier t = t
end

let kind : _ t = (module Kind)
let mode : _ t = (module Mode)
let modality : _ t = (module Modality)
