open! Stdppx
open! Import

module type S = sig
  type identifier
  type binding

  module Binding : Binding.S with type t = binding and type identifier = identifier
  module Identifier : Identifier.S with type t = identifier

  val bindings : (identifier * binding list) list
end

type ('identifier, 'binding) t =
  (module S with type identifier = 'identifier and type binding = 'binding)

module M
    (Identifier : Identifier.S)
    (Binding : Binding.S with type identifier := Identifier.t) =
struct
  type nonrec t = (Identifier.t, Binding.t) t
end

let find_all_dups (type a) list ~compare =
  let module Set =
    Set.Make (struct
      type t = a

      let compare = compare
    end)
  in
  let _, dups =
    List.fold_left list ~init:(Set.empty, Set.empty) ~f:(fun (seen, dups) elt ->
      if Set.mem elt dups
      then seen, dups
      else if Set.mem elt seen
      then seen, Set.add elt dups
      else Set.add elt seen, dups)
  in
  Set.to_list dups
;;

let create
  (type identifier binding node)
  ((module Identifier) : identifier Identifier.t)
  ((module Binding) : (binding, identifier, node) Binding.t)
  bindings
  =
  match find_all_dups bindings ~compare:(fun (a, _) (b, _) -> Identifier.compare a b) with
  | _ :: _ as dups ->
    let dups = List.map dups ~f:fst in
    Error
      (Sexp.message
         "duplicate identifiers"
         [ "", List (List.map ~f:Identifier.sexp_of_t dups) ])
  | [] ->
    (match
       List.filter_map bindings ~f:(fun (id, bindings) ->
         let ids = List.map ~f:Binding.to_mangled_identifier bindings in
         match find_all_dups ids ~compare:Identifier.compare with
         | _ :: _ as dups ->
           Some
             (Sexp.message
                "duplicate bindings for single identifier"
                [ ( ""
                  , List
                      [ Identifier.sexp_of_t id; sexp_of_list Identifier.sexp_of_t dups ]
                  )
                ])
         | [] -> None)
     with
     | _ :: _ :: _ as errors -> Error (Sexp.message "multiple errors" [ "", List errors ])
     | [ error ] -> Error error
     | [] ->
       Ok
         ((module struct
            type nonrec identifier = identifier
            type nonrec binding = binding

            module Binding = Binding
            module Identifier = Identifier

            let bindings = bindings
          end)
          : M(Identifier)(Binding).t))
;;

let empty id binding =
  match create id binding [] with
  | Ok t -> t
  | Error sexp -> failwith (Sexp.to_string sexp)
;;

let vars (type identifier binding) ((module T) : (identifier, binding) t) =
  List.map ~f:fst T.bindings
;;

module Instance = struct
  type ('id, 'binding) t = ('id, 'binding) Identifier.map

  module M
      (Identifier : Identifier.S)
      (Binding : Binding.S with type identifier := Identifier.t) =
  struct
    type nonrec t = (Identifier.t, Binding.t) t
  end
end

let stable_dedup list ~compare =
  List.mapi list ~f:(fun i x -> i, x)
  |> List.sort_uniq ~cmp:(fun (_, x) (_, y) -> compare x y)
  |> List.sort ~cmp:(fun (i, _) (j, _) -> Int.compare i j)
  |> List.map ~f:snd
;;

let instantiate
  (type identifier binding)
  ((module T) : (identifier, binding) t)
  ~find_identifier
  =
  List.map T.bindings ~f:(fun (id, bindings) ->
    List.map bindings ~f:(fun binding -> id, T.Binding.resolve binding ~find_identifier)
    |> stable_dedup ~compare:(fun (_, a) (_, b) -> T.Binding.compare a b))
  |> List.fold_right ~init:[ [] ] ~f:(fun alist tails ->
    List.concat_map alist ~f:(fun head -> List.map tails ~f:(fun tail -> head :: tail)))
  |> List.map ~f:T.Identifier.Map.of_list
;;
