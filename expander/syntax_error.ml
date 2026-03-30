open! Stdppx
module Error = Astlib.Location.Error

type t = Error.t

let createf ~loc fmt =
  Format.kasprintf
    (fun txt -> Error.make { loc = { loc with loc_ghost = true }; txt } ~sub:[])
    fmt
;;

let combine t list =
  Error.make
    (Error.main_msg t)
    ~sub:
      (Error.sub_msgs t
       @ List.concat_map list ~f:(fun err -> Error.main_msg err :: Error.sub_msgs err))
;;

(* Once we import ppxlib 0.37, ppxlib location errors will be exposed as equivalent to [t]
   and we won't need to drop sub-messages like this. Fortunately, the cases we're using
   this for are just errors from [Attribute.*_res] functions, which probably do not have
   sub-messages. *)
let loc_of_ppxlib_error err : string Ppxlib.loc =
  { loc = Ppxlib.Location.Error.get_location err
  ; txt = Ppxlib.Location.Error.message err
  }
;;

let of_location_errors (err, errs) =
  Error.make (loc_of_ppxlib_error err) ~sub:(List.map ~f:loc_of_ppxlib_error errs)
;;

let to_extension = Ppxlib_ast.Location_error.to_extension
