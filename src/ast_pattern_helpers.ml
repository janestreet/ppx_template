open! Stdppx
open! Import
open Language
open Ast_pattern

type ('a, 'node) pat = { pat : 'b. unit -> ('node, 'a -> 'b, 'b) Ast_pattern.t }
[@@unboxed]

let map_pat { pat } ~f = { pat = (fun () -> map1 (pat ()) ~f) }
let at_most_one_pattern p = p ^:: nil ||| map0 nil ~f:[]
let at_most_one_eval p = pstr (at_most_one_pattern (pstr_eval p nil))

let ident' type_ =
  { pat =
      (fun () ->
        pexp_ident
          (map1' (lident __) ~f:(fun loc ident ->
             match ident with
             | "@" | "=" ->
               (* This helps break parsing ambiguity between punning and alternate forms
                  of alloc-poly (otherwise, [[@@alloc a = heap]] can be parsed as a punned
                  binding with the identifiers [( = )], [a], and [heap]). *)
               Ppxlib__.Ast_pattern0.fail loc ("Invalid ppx_template identifier: " ^ ident)
             | _ -> { txt = { Identifier.type_; ident }; loc })))
  }
;;

let ident type_ = map_pat (ident' type_) ~f:Loc.txt
let ident_pattern type_ = map_pat (ident type_) ~f:(fun ident -> Pattern.Identifier ident)

let ident_expr type_ =
  map_pat (ident' type_) ~f:(Loc.map ~f:(fun ident -> Expression.Identifier ident))
;;

let one_or_many a b = map1 a ~f:(fun x -> [ x ]) ||| b
let tuple_or_one p = one_or_many p (pexp_tuple (many p))

let one_or_many_as_list { pat } =
  one_or_many
    (pat ())
    (map2 (pexp_apply (pat ()) (many (pair nolabel (pat ())))) ~f:List.cons)
;;

(* Parses an [expression] of the form [l = (a, b)] as ["l", [ "a"; "b" ]]. *)
let binding pat expr mangle =
  pexp_apply
    (pexp_ident (lident (string "=")))
    (pair nolabel (pat.pat ()) ^:: pair nolabel (tuple_or_one (expr.pat ())) ^:: nil)
  |> map2 ~f:(fun pattern expressions : _ Binding.t -> { pattern; expressions; mangle })
;;

(* Parses an [expression] of the form [a] as [<generated symbol>, [ "a" ]]. *)
let punned_binding expr mangle =
  map_pat expr ~f:(fun expr : _ Binding.t ->
    let ident = Ppxlib.gen_symbol ~prefix:"binding" () in
    let type_ = Expression.type_ expr.txt in
    let pattern = Pattern.Identifier { ident; type_ } in
    { pattern; expressions = [ expr ]; mangle })
;;

let binding_poly ~pattern_pat ~expression_pat ~mangle ~mangle_type =
  tuple_or_one (binding pattern_pat expression_pat mangle)
  ||| one_or_many_as_list (punned_binding expression_pat mangle)
  |> at_most_one_eval
  |> map1 ~f:(fun bindings ->
    Attributes_intf.Definitions.Poly.Poly (mangle_type, bindings))
;;

let binding_mono binding =
  binding
  |> map_pat ~f:(Loc.map ~f:(fun expr -> Expression.Basic.P expr))
  |> one_or_many_as_list
  |> at_most_one_eval
;;

let alloc_pattern =
  { pat =
      (fun () ->
        pexp_apply
          (pexp_ident (lident (string "@")))
          (no_label ((ident_pattern Type.alloc).pat ())
           ^:: no_label ((ident_pattern Type.mode).pat ())
           ^:: nil)
        |> map2 ~f:(fun alloc mode -> Pattern.Tuple2 (alloc, mode)))
  }
;;

let alloc_expr =
  { pat =
      (fun () ->
        pexp_apply
          (pexp_ident (lident (string "@")))
          (no_label ((ident_expr Type.alloc).pat ())
           ^:: no_label ((ident_expr Type.mode).pat ())
           ^:: nil)
        |> map2' ~f:(fun loc alloc mode ->
          { txt = Expression.Tuple2 (alloc.txt, mode.txt); loc })
        ||| (ident_expr Type.(tuple2 alloc mode)).pat ())
  }
;;

let kind_expr =
  let report_syntax_error ~loc =
    Location.raise_errorf
      ~loc
      "expected a kind abbreviation, product of kinds, or kind with a mod"
  in
  let rec of_expr : expression -> Type.kind Expression.t = function
    | { pexp_desc = Pexp_ident { txt = Lident ident; _ }; _ } ->
      Identifier { type_ = Type.kind; ident }
    | [%expr [%e? lhs] & [%e? rhs]] ->
      let lhs = of_expr lhs in
      let rhs =
        match of_expr rhs with
        | (Identifier _ | Kind_mod _) as rhs -> [ rhs ]
        | Kind_product rhs -> rhs
      in
      Kind_product (lhs :: rhs)
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
          | { pexp_desc = Pexp_ident { txt = Lident modifier; _ }; _ } ->
            Expression.Identifier { ident = modifier; type_ = Basic Modality }
          | { pexp_loc = loc; _ } -> report_syntax_error ~loc)
      in
      Kind_mod (base, modifiers)
    | { pexp_loc = loc; _ } -> report_syntax_error ~loc
  in
  { pat =
      (fun () ->
        Ast_pattern.of_func (fun (_ : Ast_pattern.context) loc expr k ->
          k { txt = of_expr expr; loc }))
  }
;;
