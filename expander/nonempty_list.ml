open! Stdppx
module List = List_with_helpers
include Nonempty_list_type.Nonempty_list

let hd (hd :: _) = hd
let tl (_ :: tl) = tl
let to_list (hd :: tl) : _ list = hd :: tl

let of_list = function
  | [] -> None
  | hd :: tl -> Some (hd :: tl)
;;

let of_list_exn ?(here = Stdlib.Lexing.dummy_pos) = function
  | [] ->
    Location.raise_errorf
      ~loc:{ loc_ghost = false; loc_start = here; loc_end = here }
      "Nonempty_list.of_list_exn: empty list"
  | hd :: tl -> hd :: tl
;;

let sexp_of_t f t = sexp_of_list f (to_list t)
let create hd tl = hd :: tl
let singleton x = [ x ]
let cons x t = x :: to_list t
let length (_ :: tl) = 1 + List.length tl
let map (hd :: tl) ~f = f hd :: List.map tl ~f
let mapi (hd :: tl) ~f = f 0 hd :: List.mapi tl ~f:(fun i x -> f (i + 1) x)

let compare (x :: xs) (y :: ys) ~cmp =
  match cmp x y with
  | 0 -> List.compare ~cmp xs ys
  | r -> r
;;

let sort_uniq t ~cmp = t |> to_list |> List.sort_uniq ~cmp |> of_list_exn
let stable_dedup t ~cmp = t |> to_list |> List.stable_dedup ~cmp |> of_list_exn

let[@tail_mod_cons] rec concat_map_list xs ~f =
  match xs with
  | [] -> []
  | x :: xs ->
    let (y :: ys) = f x in
    y :: append_and_concat_map_list ys xs ~f

and[@tail_mod_cons] append_and_concat_map_list ys xs ~f =
  match ys with
  | [] -> concat_map_list xs ~f
  | y :: ys -> y :: append_and_concat_map_list ys xs ~f
;;

let[@tail_mod_cons] concat_map (x :: xs) ~f =
  let (y :: ys) = f x in
  y :: append_and_concat_map_list ys xs ~f
;;

let concat t = concat_map t ~f:Fn.id

let rev (hd :: tl) =
  let hd, tl = List.fold_left tl ~init:(hd, []) ~f:(fun (hd, tl) elt -> elt, hd :: tl) in
  hd :: tl
;;

let product outer =
  let (inner :: outer) = rev outer in
  List.fold_left
    outer
    ~init:(map inner ~f:(fun x -> [ x ]))
    ~f:(fun acc inner ->
      concat_map inner ~f:(fun hd -> map acc ~f:(fun tl -> cons hd tl)))
;;

module Or_first_error = struct
  let fold_left t ~init ~f = List.Or_first_error.fold_left (to_list t) ~init ~f

  let map (x :: xs) ~f =
    Result.bind (f x) ~f:(fun y ->
      Result.map (List.Or_first_error.map ~f xs) ~f:(fun ys -> y :: ys))
  ;;

  let concat_map t ~f = map t ~f |> Result.map ~f:concat
end
