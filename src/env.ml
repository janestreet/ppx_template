open! Base
open! Import

type t =
  { kinds : Bindings.Instance.M(Identifier.Kind).t
  ; modes : Bindings.Instance.M(Identifier.Mode).t
  }

let create ~kinds ~modes = { kinds; modes }
let kinds t = t.kinds
let modes t = t.modes

let empty =
  { kinds = Map.empty (module Identifier.Kind)
  ; modes = Map.empty (module Identifier.Mode)
  }
;;

let find t id ~project = Map.find (project t) id |> Option.value ~default:id
let find_kind = find ~project:kinds
let find_mode = find ~project:modes

let lookup_and_set_all ~current_env ~uninterpreted_env =
  let merge ~project ~find =
    Map.merge_skewed
      (project current_env)
      (Map.map ~f:(find current_env) (project uninterpreted_env))
      ~combine:(fun ~key:_ _ binding -> binding)
  in
  { kinds = merge ~project:kinds ~find:find_kind
  ; modes = merge ~project:modes ~find:find_mode
  }
;;
