open! Stdppx
include List

let partition_map t ~f = partition_map f t
let fold_left list ~init ~f = fold_left list ~init ~f

let[@tail_mod_cons] rec concat_map xs ~f =
  match xs with
  | [] -> []
  | x :: xs -> append_and_concat_map (f x) xs ~f

and[@tail_mod_cons] append_and_concat_map ys xs ~f =
  match ys with
  | [] -> concat_map xs ~f
  | y :: ys -> y :: append_and_concat_map ys xs ~f
;;

let concat l = concat_map l ~f:Fn.id

let stable_dedup list ~cmp =
  mapi list ~f:(fun i x -> i, x)
  |> sort_uniq ~cmp:(fun (_, x) (_, y) -> cmp x y)
  |> sort ~cmp:(fun (i, _) (j, _) -> Int.compare i j)
  |> map ~f:snd
;;

module Or_first_error = struct
  let rec fold_left list ~init ~f =
    match list with
    | [] -> Ok init
    | x :: xs ->
      (match f init x with
       | Error _ as err -> err
       | Ok init -> fold_left xs ~init ~f)
  ;;

  let iter list ~f = fold_left list ~init:() ~f:(fun () x -> f x)

  let map list ~f =
    fold_left list ~init:[] ~f:(fun ys x -> Result.map (f x) ~f:(fun y -> y :: ys))
    |> Result.map ~f:rev
  ;;

  let mapi list ~f =
    fold_left list ~init:(0, []) ~f:(fun (i, ys) x ->
      Result.map (f i x) ~f:(fun y -> i + 1, y :: ys))
    |> Result.map ~f:(fun (_, acc) -> rev acc)
  ;;

  let filter_map list ~f = map list ~f |> Result.map ~f:filter_opt
  let concat_map list ~f = map list ~f |> Result.map ~f:concat
end
