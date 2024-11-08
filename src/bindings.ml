open! Base
open! Import

type ('id, 'binding, 'cmp) t =
  { bindings : ('id, 'binding list) List.Assoc.t
  ; comparator : ('id, 'cmp) Comparator.Module.t
  }

module M
    (Identifier : Identifier.S)
    (Binding : Binding.S with type identifier := Identifier.t) =
struct
  type nonrec t = (Identifier.t, Binding.t, Identifier.comparator_witness) t
end

let create
  (type id cmp binding node)
  ((module Id) : (id, cmp) Identifier.t)
  ((module Binding) : (binding, id, node) Binding.t)
  bindings
  =
  match List.find_all_dups bindings ~compare:(Comparable.lift Id.compare ~f:fst) with
  | _ :: _ as dups ->
    let dups = List.map dups ~f:fst in
    Or_error.error "duplicate identifiers" dups (List.sexp_of_t Id.sexp_of_t)
  | [] ->
    (match
       List.map bindings ~f:(fun (id, bindings) ->
         let ids = List.map ~f:Binding.to_mangled_identifier bindings in
         match List.find_all_dups ids ~compare:Id.compare with
         | _ :: _ as dups ->
           Or_error.error
             "duplicate bindings for single identifier"
             (id, dups)
             (fun (id, dups) ->
                List [ Id.sexp_of_t id; List.sexp_of_t Id.sexp_of_t dups ])
         | [] -> Ok ())
       |> Or_error.combine_errors_unit
     with
     | Error _ as result -> result
     | Ok () -> Ok { bindings; comparator = (module Id) })
;;

let empty (type a cmp) ((module Id) : (a, cmp) Identifier.t) =
  { bindings = []; comparator = (module Id) }
;;

let vars t = List.map ~f:fst t.bindings

module Instance = struct
  type ('id, 'binding, 'cmp) t = ('id, 'binding, 'cmp) Map.t

  module M
      (Identifier : Identifier.S)
      (Binding : Binding.S with type identifier := Identifier.t) =
  struct
    type nonrec t = (Identifier.t, Binding.t, Identifier.comparator_witness) t
  end
end

let instantiate t =
  List.map t.bindings ~f:(fun (id, bindings) ->
    List.map bindings ~f:(fun binding -> id, binding))
  |> List.Cartesian_product.all
  |> List.map ~f:(Map.of_alist_exn t.comparator)
;;
