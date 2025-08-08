open! Stdppx
open! Import
open Language.Untyped
open Ast_pattern

(** A [('a, 'node) pat] constructs an [Ast_pattern] that parse ['node]s and produce ['a]s.
    This is used to get around the value restriction when the same pattern needs to be
    used multiple times. *)
type ('a, 'node) pat = { pat : 'b. unit -> ('node, 'a -> 'b, 'b) Ast_pattern.t }
[@@unboxed]

let map_pat { pat } ~f = { pat = (fun () -> map1 (pat ()) ~f) }
let at_most_one_pattern p = p ^:: nil ||| map0 nil ~f:[]
let at_most_one_eval p = pstr (at_most_one_pattern (pstr_eval p nil))

let ident' =
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
             | _ -> { txt = { Identifier.ident }; loc })))
  }
;;

let ident = map_pat ident' ~f:Loc.txt
let ident_pattern = map_pat ident ~f:(fun ident -> Pattern.Identifier ident)
let ident_expr = map_pat ident' ~f:(Loc.map ~f:(fun ident -> Expression.Identifier ident))
let one_or_many a b = map1 a ~f:(fun x -> [ x ]) ||| b
let tuple_or_one p = pexp_tuple (many p) ||| map1 p ~f:(fun x -> [ x ])
let one_or_tuple p = one_or_many p (pexp_tuple (many p))

let one_or_many_as_list { pat } =
  one_or_many
    (pat ())
    (map2 (pexp_apply (pat ()) (many (pair nolabel (pat ())))) ~f:List.cons)
;;

let alloc_pattern =
  { pat =
      (fun () ->
        pexp_apply
          (pexp_ident (lident (string "@")))
          (no_label (ident_pattern.pat ()) ^:: no_label (ident_pattern.pat ()) ^:: nil)
        |> map2 ~f:(fun alloc mode -> Pattern.Tuple [ alloc; mode ]))
  }
;;

let expr =
  let report_syntax_error ~loc fmt =
    Location.raise_errorf ~loc (Stdlib.( ^^ ) "[ppx_template] syntax error: " fmt)
  in
  let rec of_expr : expression -> Expression.t =
    fun ({ pexp_desc; pexp_loc = loc; pexp_attributes; pexp_loc_stack = _ } as expr) ->
    let () =
      match pexp_attributes with
      | attr :: _ ->
        report_syntax_error ~loc:attr.attr_loc "attributes are not allowed here"
      | [] -> ()
    in
    match expr, Ppxlib_jane.Shim.Expression_desc.of_parsetree ~loc pexp_desc with
    | _, Pexp_ident { txt = Lident ident; _ } -> Identifier { ident }
    | [%expr [%e? lhs] & [%e? rhs]], _ ->
      let lhs = of_expr lhs in
      let rhs =
        match of_expr rhs with
        | Kind_product rhs ->
          (* Special case: because we're using the expression language to represent
             kinds, we need to cheat and parse [a & b & c === a & (b & c)] as a flat
             kind. *)
          rhs
        | (Tuple _ | Identifier _ | Kind_mod _) as rhs -> [ rhs ]
      in
      Kind_product (lhs :: rhs)
    | [%expr [%e? base] mod [%e? modifiers_exp]], _ ->
      let base = of_expr base in
      let modifier_exps =
        match modifiers_exp with
        | { pexp_desc = Pexp_apply (modifiers_hd, modifiers_tl); _ } ->
          let modifiers_tl =
            List.map modifiers_tl ~f:(fun (label, modifier) ->
              match label with
              | Nolabel -> modifier
              | Labelled _ | Optional _ ->
                report_syntax_error
                  ~loc:modifier.pexp_loc
                  "unexpected label on kind modifier")
          in
          modifiers_hd :: modifiers_tl
        | modifiers_hd -> [ modifiers_hd ]
      in
      let modifiers = List.map modifier_exps ~f:of_expr in
      Kind_mod (base, modifiers)
    | [%expr [%e? lhs] @ [%e? rhs]], _ -> Tuple [ of_expr lhs; of_expr rhs ]
    | _, Pexp_tuple lab_exprs ->
      Tuple
        (List.map lab_exprs ~f:(function
          | Some _label, { pexp_loc = loc; _ } ->
            report_syntax_error ~loc "unexpected label on tuple element"
          | None, expr -> of_expr expr))
    | _, Pexp_construct _ ->
      report_syntax_error ~loc "constructors are not allowed in template expressions"
    | _ -> report_syntax_error ~loc "invalid syntax"
  in
  { pat =
      (fun () ->
        Ast_pattern.of_func (fun (_ : Ast_pattern.context) loc expr k ->
          k { txt = of_expr expr; loc }))
  }
;;

let pattern =
  { pat =
      (fun () ->
        one_or_tuple (ident_pattern.pat ())
        |> map1 ~f:(function
          | [ pat ] -> pat
          | pats -> Pattern.Tuple pats)
        ||| alloc_pattern.pat ())
  }
;;

(* Parses an [expression] of the form [l = (a, b)] as ["l", [ "a"; "b" ]]. *)
let binding =
  { pat =
      (fun () ->
        pexp_apply
          (pexp_ident (lident (string "=")))
          (pair nolabel (pattern.pat ())
           ^:: pair nolabel (tuple_or_one (expr.pat ()))
           ^:: nil)
        |> pack2)
  }
;;

(* Parses an [expression] of the form [a] as [<generated symbol>, [ "a" ]]. *)
let punned_binding =
  map_pat expr ~f:(fun expr ->
    let ident = Ppxlib.gen_symbol ~prefix:"binding" () in
    let pattern = Pattern.Identifier { ident } in
    pattern, [ expr ])
;;

let single_ident () = pstr (pstr_eval (ident_expr.pat ()) nil ^:: nil)
let ident_expr () = ident_expr.pat ()
let multiple_idents () = expr |> one_or_many_as_list |> at_most_one_eval

let bindings () =
  one_or_tuple (binding.pat ()) ||| one_or_many_as_list punned_binding |> at_most_one_eval
;;
