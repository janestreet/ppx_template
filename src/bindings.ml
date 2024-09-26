open! Base
open! Import

type ('a, 'cmp) t =
  { bindings : ('a, 'a list) List.Assoc.t
  ; comparator : ('a, 'cmp) Comparator.Module.t
  }

module M (Identifier : Identifier.S) = struct
  type nonrec t = (Identifier.t, Identifier.comparator_witness) t
end

let create (type a cmp) ((module Id) : (a, cmp) Identifier.t) bindings =
  match List.find_all_dups bindings ~compare:(Comparable.lift Id.compare ~f:fst) with
  | _ :: _ as dups ->
    let dups = List.map dups ~f:fst in
    Or_error.error "duplicate kind variables" dups (List.sexp_of_t Id.sexp_of_t)
  | [] ->
    (match
       List.map bindings ~f:(fun (var, bindings) ->
         match List.find_all_dups bindings ~compare:Id.compare with
         | _ :: _ as dups ->
           Or_error.error
             "duplicate kinds for single kind variable"
             (var, dups)
             (fun (var, dups) ->
                List [ Id.sexp_of_t var; List.sexp_of_t Id.sexp_of_t dups ])
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
  type ('a, 'cmp) t = ('a, 'a, 'cmp) Map.t

  module M (Identifier : Identifier.S) = struct
    type nonrec t = (Identifier.t, Identifier.comparator_witness) t
  end
end

let instantiate t =
  List.map t.bindings ~f:(fun (var, bindings) ->
    List.map bindings ~f:(fun binding -> var, binding))
  |> List.Cartesian_product.all
  |> List.map ~f:(Map.of_alist_exn t.comparator)
;;
