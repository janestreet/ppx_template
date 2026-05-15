open! Stdppx
module List = List_with_helpers
include Result

module Let_syntax = struct
  let ( let* ) t f = bind t ~f
  let ( let+ ) t f = map t ~f
  let ( >>= ) = ( >>= )
  let ( >>| ) = ( >>| )
end

let map_error t ~f =
  match t with
  | Ok _ as ok -> ok
  | Error e -> Error (f e)
;;

let all_errors_of_list l =
  let oks, errs =
    List.partition_map l ~f:(function
      | Ok x -> Left x
      | Error x -> Right x)
  in
  match Nonempty_list.of_list errs with
  | None -> Ok oks
  | Some errs -> Error errs
;;

let all_errors_of_unit_list l =
  List.filter_map l ~f:(function
    | Ok () -> None
    | Error error -> Some error)
  |> Nonempty_list.of_list
  |> function
  | None -> Ok ()
  | Some errs -> Error errs
;;

let syntax_errors_of_list l = all_errors_of_list l |> map_error ~f:Syntax_error.combine

let syntax_errors_of_unit_list l =
  all_errors_of_unit_list l |> map_error ~f:Syntax_error.combine
;;
