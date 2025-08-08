open! Stdppx
include Result

module Let_syntax = struct
  let ( let* ) t f = bind t ~f
  let ( let+ ) t f = map t ~f
end

open Let_syntax

let all results =
  List.fold_right results ~init:(Ok []) ~f:(fun elt acc ->
    let* elt = elt in
    let+ acc = acc in
    elt :: acc)
;;

let collect_errors results =
  List.fold_right results ~init:(Ok []) ~f:(fun elt acc ->
    match elt, acc with
    | Ok x, Ok xs -> Ok (x :: xs)
    | Ok _, (Error _ as errs) -> errs
    | Error err, Ok _ -> Error [ err ]
    | Error err, Error errs -> Error (err :: errs))
  |> function
  | Ok xs -> Ok xs
  | Error [ err ] -> Error err
  | Error errs -> Error (List errs : Sexp.t)
;;
