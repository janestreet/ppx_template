open! Stdppx

include module type of struct
  include Result
end

module Let_syntax : sig
  val ( let* )
    :  ('ok1, 'err) result
    -> ('ok1 -> ('ok2, 'err) result)
    -> ('ok2, 'err) result

  val ( >>= )
    :  ('ok1, 'err) result
    -> ('ok1 -> ('ok2, 'err) result)
    -> ('ok2, 'err) result

  val ( let+ ) : ('ok1, 'err) result -> ('ok1 -> 'ok2) -> ('ok2, 'err) result
  val ( >>| ) : ('ok1, 'err) result -> ('ok1 -> 'ok2) -> ('ok2, 'err) result
end

val map_error : ('a, 'b) result -> f:('b -> 'c) -> ('a, 'c) result

val all_errors_of_list
  :  ('ok, 'err) result list
  -> ('ok list, 'err Nonempty_list.t) result

val syntax_errors_of_list
  :  ('a, Syntax_error.t) result list
  -> ('a list, Syntax_error.t) result

val syntax_errors_of_unit_list
  :  (unit, Syntax_error.t) result list
  -> (unit, Syntax_error.t) result
