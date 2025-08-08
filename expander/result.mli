open! Stdppx

include module type of struct
  include Result
end

module Let_syntax : sig
  val ( let* )
    :  ('ok1, 'err) result
    -> ('ok1 -> ('ok2, 'err) result)
    -> ('ok2, 'err) result

  val ( let+ ) : ('ok1, 'err) result -> ('ok1 -> 'ok2) -> ('ok2, 'err) result
end

val all : ('a, 'err) result list -> ('a list, 'err) result
val collect_errors : ('a, Sexp.t) result list -> ('a list, Sexp.t) result
