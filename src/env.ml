open! Base
open! Import

type t =
  { kinds : Bindings.Instance.M(Identifier.Kind)(Binding.Kind).t
  ; modes : Bindings.Instance.M(Identifier.Mode)(Binding.Mode).t
  }

let create ~kinds ~modes = { kinds; modes }
let kinds t = t.kinds
let modes t = t.modes

let empty =
  { kinds = Map.empty (module Identifier.Kind)
  ; modes = Map.empty (module Identifier.Mode)
  }
;;

let find t id ~project = Map.find (project t) id
let find_kind = find ~project:kinds
let find_mode = find ~project:modes
let find_kind_exn t kind = find_kind t kind |> Option.value_exn
let find_mode_exn t mode = find_mode t mode |> Option.value_exn

let lookup_and_set_all ~current_env ~uninterpreted_env =
  let merge
    (type binding id node)
    ((module Binding) : (binding, id, node) Binding.t)
    ~project
    ~find
    =
    Map.merge_skewed
      (project current_env)
      (Map.map
         (project uninterpreted_env)
         ~f:(Binding.resolve ~find_identifier:(find current_env)))
      ~combine:(fun ~key:_ _ binding -> binding)
  in
  { kinds = merge Binding.kind ~project:kinds ~find:find_kind
  ; modes = merge Binding.mode ~project:modes ~find:find_mode
  }
;;
