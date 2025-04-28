open! Stdppx
open! Import

type t =
  { kinds : Bindings.Instance.M(Identifier.Kind)(Binding.Kind).t
  ; modes : Bindings.Instance.M(Identifier.Mode)(Binding.Mode).t
  ; modalities : Bindings.Instance.M(Identifier.Modality)(Binding.Modality).t
  }

let create ~kinds ~modes ~modalities = { kinds; modes; modalities }

let empty =
  { kinds = Identifier.Kind.Map.empty
  ; modes = Identifier.Mode.Map.empty
  ; modalities = Identifier.Modality.Map.empty
  }
;;

let find_kind t id = Identifier.Kind.Map.find_opt id t.kinds
let find_mode t id = Identifier.Mode.Map.find_opt id t.modes
let find_modality t id = Identifier.Modality.Map.find_opt id t.modalities
let find_kind_exn t id = Identifier.Kind.Map.find id t.kinds
let find_mode_exn t id = Identifier.Mode.Map.find id t.modes
let find_modality_exn t id = Identifier.Modality.Map.find id t.modalities

let conflate_mode_as_modality t { txt = mode; loc } =
  match Identifier.Mode.Map.find_opt mode t.modes with
  | None ->
    Location.raise_errorf
      ~loc
      "Tried to conflate an unbound mode %s"
      (Identifier.Mode.to_string mode)
  | Some mode' ->
    let modality = Identifier.Mode.to_string mode |> Identifier.Modality.of_string in
    let modality' = Identifier.Mode.to_string mode' |> Identifier.Modality.of_string in
    { t with modalities = Identifier.Modality.Map.add modality modality' t.modalities }
;;

let conflate_modality_as_mode t { txt = modality; loc } =
  match Identifier.Modality.Map.find_opt modality t.modalities with
  | None ->
    Location.raise_errorf
      ~loc
      "Tried to conflate an unbound modality %s"
      (Identifier.Modality.to_string modality)
  | Some modality' ->
    let mode = Identifier.Modality.to_string modality |> Identifier.Mode.of_string in
    let mode' = Identifier.Modality.to_string modality' |> Identifier.Mode.of_string in
    { t with modes = Identifier.Mode.Map.add mode mode' t.modes }
;;

let conflate t ~modes ~modalities =
  let t = List.fold_left modes ~init:t ~f:conflate_mode_as_modality in
  List.fold_left modalities ~init:t ~f:conflate_modality_as_mode
;;

let set_all ~current_env ~uninterpreted_env =
  { kinds =
      Identifier.Kind.Map.union
        (fun _ _ x -> Some x)
        current_env.kinds
        uninterpreted_env.kinds
  ; modes =
      Identifier.Mode.Map.union
        (fun _ _ x -> Some x)
        current_env.modes
        uninterpreted_env.modes
  ; modalities =
      Identifier.Modality.Map.union
        (fun _ _ x -> Some x)
        current_env.modalities
        uninterpreted_env.modalities
  }
;;
