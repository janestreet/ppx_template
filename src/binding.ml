open! Base
open! Import
include Binding_intf.Definitions

module Kind = struct
  type identifier = Identifier.Kind.t
  type node = jkind_annotation

  type t =
    | Abbreviation of Identifier.Kind.t
    | Product of t list

  let pattern () =
    let rec of_expr = function
      | { pexp_desc = Pexp_ident { txt = Lident kind; _ }; _ } ->
        Abbreviation (Identifier.Kind.of_string kind)
      | [%expr [%e? lhs] & [%e? rhs]] ->
        let lhs = of_expr lhs in
        let rhs =
          match of_expr rhs with
          | Abbreviation _ as rhs -> [ rhs ]
          | Product rhs -> rhs
        in
        Product (lhs :: rhs)
      | { pexp_loc = loc; _ } ->
        Location.raise_errorf ~loc "expected a kind abbreviation or product kind"
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
  ;;

  let rec to_node ~loc t : jkind_annotation =
    { pjkind_desc =
        (match t with
         | Abbreviation kind -> Abbreviation (Identifier.Kind.to_string kind)
         | Product ts ->
           Product
             (List.mapi ts ~f:(fun i t ->
                to_node ~loc:{ loc with loc_ghost = loc.loc_ghost || i > 0 } t)))
    ; pjkind_loc = loc
    }
  ;;

  let rec to_mangled_identifier = function
    | Abbreviation kind -> kind
    | Product ts ->
      List.map ts ~f:(to_mangled_identifier >> Identifier.Kind.to_string)
      |> String.concat ~sep:"_"
      |> Printf.sprintf "'%s'"
      |> Identifier.Kind.of_string
  ;;
end

module Mode = struct
  type identifier = Identifier.Mode.t
  type node = mode
  type t = Identifier.Mode.t

  let pattern () =
    Ast_pattern.(pexp_ident (map1 (lident __) ~f:Identifier.Mode.of_string))
  ;;

  let resolve t ~find_identifier = find_identifier t |> Option.value ~default:t
  let to_node ~loc:(_ : location) t = Mode (Identifier.Mode.to_string t)
  let to_mangled_identifier t = t
end

let kind : _ t = (module Kind)
let mode : _ t = (module Mode)
