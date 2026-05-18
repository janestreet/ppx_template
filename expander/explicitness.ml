open Stdppx

type t =
  | Drop_axis_if_all_defaults
  | Explicit
  | Explicit_plus_unmangled

type explicitness = t

let equal : t -> t -> bool = Poly.equal

let to_string = function
  | Explicit -> "explicit"
  | Explicit_plus_unmangled -> "explicit_plus_unmangled"
  | Drop_axis_if_all_defaults -> "default"
;;

module With = struct
  type 'a t =
    { explicitness : explicitness
    ; what : 'a
    }

  let explicitness t = t.explicitness
  let what t = t.what
  let map t ~f = { t with what = f t.what }

  let ok { explicitness; what } =
    match what with
    | Error _ as err -> err
    | Ok what -> Ok { explicitness; what }
  ;;

  let map_result t ~f = map t ~f |> ok

  module Export = struct
    type 'a _with_explicitness = 'a t =
      { explicitness : explicitness
      ; what : 'a
      }
  end
end

module Each = struct
  type 'a t =
    { explicit : 'a
    ; explicit_plus_unmangled : 'a
    ; drop_axis_if_all_defaults : 'a
    }

  let extract_list { explicit; explicit_plus_unmangled; drop_axis_if_all_defaults } =
    [ explicit; explicit_plus_unmangled; drop_axis_if_all_defaults ]
  ;;

  let all ts =
    let unzip3 l =
      List.fold_right l ~init:([], [], []) ~f:(fun (x, y, z) (xs, ys, zs) ->
        x :: xs, y :: ys, z :: zs)
    in
    let explicit, explicit_plus_unmangled, drop_axis_if_all_defaults =
      List.map
        ts
        ~f:(fun { explicit; explicit_plus_unmangled; drop_axis_if_all_defaults } ->
          explicit, explicit_plus_unmangled, drop_axis_if_all_defaults)
      |> unzip3
    in
    { explicit; explicit_plus_unmangled; drop_axis_if_all_defaults }
  ;;

  let create f =
    { explicit = f Explicit
    ; explicit_plus_unmangled = f Explicit_plus_unmangled
    ; drop_axis_if_all_defaults = f Drop_axis_if_all_defaults
    }
  ;;

  let combine { explicit; explicit_plus_unmangled; drop_axis_if_all_defaults }
    : (_ With.t option, _) result
    =
    match explicit, explicit_plus_unmangled, drop_axis_if_all_defaults with
    | None, None, None -> Ok None
    | Some res, None, None -> Ok (Some { explicitness = Explicit; what = res })
    | None, Some res, None ->
      Ok (Some { explicitness = Explicit_plus_unmangled; what = res })
    | None, None, Some res ->
      Ok (Some { explicitness = Drop_axis_if_all_defaults; what = res })
    | None, Some _, Some _
    | Some _, None, Some _
    | Some _, Some _, None
    | Some _, Some _, Some _ -> Error `multiple
  ;;

  let fold_map { explicit; explicit_plus_unmangled; drop_axis_if_all_defaults } ~init ~f =
    let init, explicit = f init explicit in
    let init, explicit_plus_unmangled = f init explicit_plus_unmangled in
    let init, drop_axis_if_all_defaults = f init drop_axis_if_all_defaults in
    init, { explicit; explicit_plus_unmangled; drop_axis_if_all_defaults }
  ;;

  let map t ~f = fold_map t ~init:() ~f:(fun () x -> (), f x) |> snd
end
