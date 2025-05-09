open! Core

[@@@disable_unused_warnings]

[@@@expand_inline
  module%template List : sig
    type 'a t = 'a list

    [%%template:
    [@@@alloc.default __ @ m_out = (heap, stack)]

    (* [cons] can't implement [local -> heap] *)
    [@@@mode.default m_in = (global, m_out)]

    val cons : 'a @ m_in -> 'a t @ m_in -> 'a t @ m_out [@@zero_alloc_if_local m_out]]

    [%%template:
    [@@@alloc.default a @ m_out = (heap, stack)]
    [@@@mode.default m_in = (local, global)]

    val map : 'a t @ m_in -> f:('a @ m_in -> 'a @ m_out) -> 'a t @ m_out
    [@@zero_alloc_if_stack a]

    val fold
      :  'a t @ m_in
      -> init:'acc @ m_out
      -> f:('acc @ m_out -> 'a @ m_in -> 'acc @ m_out)
      -> 'acc @ m_out
    [@@zero_alloc_if_local m_out] [@@zero_alloc_if_stack a]]
  end = struct
    type 'a t = 'a list

    [%%template
    [@@@alloc.default a @ m_out = (heap, stack)]
    [@@@mode.default m_in = (global, m_out)]

    let cons (hd @ m_in) (tl @ m_in) = hd :: tl [@exclave_if_stack a]]

    [%%template
    [@@@alloc.default a @ m_out = (heap, stack)]
    [@@@mode.default m_in = (local, global)]

    let rec map (t @ m_in) ~f =
      match[@exclave_if_stack a] t with
      | [] -> []
      | hd :: tl ->
        (f [@zero_alloc_if_local m_out assume]) hd :: (map [@mode m_in] [@alloc a]) tl ~f
    ;;

    let rec fold (t @ m_in) ~(init @ m_out) ~f =
      match[@exclave_if_stack a] t with
      | [] -> init
      | hd :: tl ->
        (fold [@mode m_in] [@alloc a])
          tl
          ~init:((f [@zero_alloc_if_local m_out assume]) init hd)
          ~f
    ;;]
  end]

module List : sig
  type 'a t = 'a list

  include sig
    include sig
      val cons : 'a @ global -> 'a t @ global -> 'a t @ global
    end
  end

  include sig
    include sig
      val cons__stack : 'a @ global -> 'a t @ global -> local_ 'a t [@@zero_alloc]
    end

    include sig
      val cons__local__stack : local_ 'a -> local_ 'a t -> local_ 'a t [@@zero_alloc]
    end
  end

  include sig
    include sig
      val map__local : local_ 'a t -> f:(local_ 'a -> 'a @ global) -> 'a t @ global

      val fold__local
        :  local_ 'a t
        -> init:'acc @ global
        -> f:('acc @ global -> local_ 'a -> 'acc @ global)
        -> 'acc @ global
    end

    include sig
      val map : 'a t @ global -> f:('a @ global -> 'a @ global) -> 'a t @ global

      val fold
        :  'a t @ global
        -> init:'acc @ global
        -> f:('acc @ global -> 'a @ global -> 'acc @ global)
        -> 'acc @ global
    end
  end

  include sig
    include sig
      val map__local__stack : local_ 'a t -> f:(local_ 'a -> local_ 'a) -> local_ 'a t
      [@@zero_alloc]

      val fold__local__stack
        :  local_ 'a t
        -> init:local_ 'acc
        -> f:(local_ 'acc -> local_ 'a -> local_ 'acc)
        -> local_ 'acc
      [@@zero_alloc]
    end

    include sig
      val map__stack : 'a t @ global -> f:('a @ global -> local_ 'a) -> local_ 'a t
      [@@zero_alloc]

      val fold__stack
        :  'a t @ global
        -> init:local_ 'acc
        -> f:(local_ 'acc -> 'a @ global -> local_ 'acc)
        -> local_ 'acc
      [@@zero_alloc]
    end
  end
end = struct
  type 'a t = 'a list

  include struct
    include struct
      let cons (hd @ global) (tl @ global) = hd :: tl
    end
  end

  include struct
    include struct
      let cons__stack (hd @ global) (tl @ global) = exclave_ hd :: tl
    end

    include struct
      let cons__local__stack (local_ hd) (local_ tl) = exclave_ hd :: tl
    end
  end

  include struct
    include struct
      let rec map__local (local_ t) ~f =
        match t with
        | [] -> []
        | hd :: tl -> f hd :: map__local tl ~f
      ;;

      let rec fold__local (local_ t) ~(init @ global) ~f =
        match t with
        | [] -> init
        | hd :: tl -> fold__local tl ~init:(f init hd) ~f
      ;;
    end

    include struct
      let rec map (t @ global) ~f =
        match t with
        | [] -> []
        | hd :: tl -> f hd :: map tl ~f
      ;;

      let rec fold (t @ global) ~(init @ global) ~f =
        match t with
        | [] -> init
        | hd :: tl -> fold tl ~init:(f init hd) ~f
      ;;
    end
  end

  include struct
    include struct
      let rec map__local__stack (local_ t) ~f = exclave_
        match t with
        | [] -> []
        | hd :: tl -> (f [@zero_alloc assume]) hd :: map__local__stack tl ~f
      ;;

      let rec fold__local__stack (local_ t) ~(local_ init) ~f = exclave_
        match t with
        | [] -> init
        | hd :: tl -> fold__local__stack tl ~init:((f [@zero_alloc assume]) init hd) ~f
      ;;
    end

    include struct
      let rec map__stack (t @ global) ~f = exclave_
        match t with
        | [] -> []
        | hd :: tl -> (f [@zero_alloc assume]) hd :: map__stack tl ~f
      ;;

      let rec fold__stack (t @ global) ~(local_ init) ~f = exclave_
        match t with
        | [] -> init
        | hd :: tl -> fold__stack tl ~init:((f [@zero_alloc assume]) init hd) ~f
      ;;
    end
  end
end

[@@@end]

(* [[@alloc]] with no payload disables the default. *)

[@@@expand_inline
  [%%template
  [@@@alloc.default a @ m = (heap, stack)]

  let[@alloc] f x = x]]

include struct
  let f x = x
end

include struct
  let f x = x
end

[@@@end]
