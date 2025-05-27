open! Core

[@@@disable_unused_warnings]

[@@@expand_inline
  module%template List : sig
    type 'a t = 'a list

    [%%template:
    [@@@alloc.default __ @ m_out = (heap_global, stack_local)]

    (* [cons] can't implement [local -> heap] *)
    [@@@mode.default m_in = (global, m_out)]

    val cons : 'a -> 'a t -> 'a t [@@zero_alloc_if_local m_out]]

    [%%template:
    [@@@alloc.default a @ m_out = (heap_global, stack_local)]
    [@@@mode.default m_in = (local, global)]

    val map : 'a t -> f:('a -> 'a) -> 'a t [@@zero_alloc_if_stack a]

    val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
    [@@zero_alloc_if_local m_out] [@@zero_alloc_if_stack a]]
  end = struct
    type 'a t = 'a list

    [%%template
    [@@@alloc.default a @ m_out = (heap_global, stack_local)]
    [@@@mode.default m_in = (global, m_out)]

    let cons hd tl = hd :: tl [@exclave_if_stack a]]

    [%%template
    [@@@alloc.default a @ m_out = (heap_global, stack_local)]
    [@@@mode.default m_in = (local, global)]

    let rec map t ~f =
      match[@exclave_if_stack a] t with
      | [] -> []
      | hd :: tl ->
        (f [@zero_alloc_if_local m_out assume]) hd :: (map [@mode m_in] [@alloc a]) tl ~f
    ;;

    let rec fold t ~init ~f =
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
      val cons : 'a -> 'a t -> 'a t
    end
  end

  include sig
    include sig
      val cons__stack : 'a -> 'a t -> 'a t [@@zero_alloc]
    end

    include sig
      val cons__local__stack : 'a -> 'a t -> 'a t [@@zero_alloc]
    end
  end

  include sig
    include sig
      val map__local : 'a t -> f:('a -> 'a) -> 'a t
      val fold__local : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
    end

    include sig
      val map : 'a t -> f:('a -> 'a) -> 'a t
      val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
    end
  end

  include sig
    include sig
      val map__local__stack : 'a t -> f:('a -> 'a) -> 'a t [@@zero_alloc]

      val fold__local__stack : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
      [@@zero_alloc]
    end

    include sig
      val map__stack : 'a t -> f:('a -> 'a) -> 'a t [@@zero_alloc]
      val fold__stack : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc [@@zero_alloc]
    end
  end
end = struct
  type 'a t = 'a list

  include struct
    include struct
      let cons hd tl = hd :: tl
    end
  end

  include struct
    include struct
      let cons__stack hd tl = hd :: tl
    end

    include struct
      let cons__local__stack hd tl = hd :: tl
    end
  end

  include struct
    include struct
      let rec map__local t ~f =
        match t with
        | [] -> []
        | hd :: tl -> f hd :: map__local tl ~f
      ;;

      let rec fold__local t ~init ~f =
        match t with
        | [] -> init
        | hd :: tl -> fold__local tl ~init:(f init hd) ~f
      ;;
    end

    include struct
      let rec map t ~f =
        match t with
        | [] -> []
        | hd :: tl -> f hd :: map tl ~f
      ;;

      let rec fold t ~init ~f =
        match t with
        | [] -> init
        | hd :: tl -> fold tl ~init:(f init hd) ~f
      ;;
    end
  end

  include struct
    include struct
      let rec map__local__stack t ~f =
        match t with
        | [] -> []
        | hd :: tl -> (f [@zero_alloc assume]) hd :: map__local__stack tl ~f
      ;;

      let rec fold__local__stack t ~init ~f =
        match t with
        | [] -> init
        | hd :: tl -> fold__local__stack tl ~init:((f [@zero_alloc assume]) init hd) ~f
      ;;
    end

    include struct
      let rec map__stack t ~f =
        match t with
        | [] -> []
        | hd :: tl -> (f [@zero_alloc assume]) hd :: map__stack tl ~f
      ;;

      let rec fold__stack t ~init ~f =
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
  [@@@alloc.default a @ m = (heap_global, stack_local)]

  let[@alloc] f x = x]]

include struct
  let f x = x
end

include struct
  let f x = x
end

[@@@end]

(* Further examples of using the alloc-poly syntax. *)

[@@@expand_inline
  [%%template
  (* Normal *)
  [@@@alloc.default a @ m = (heap_global, stack_local)]

  module Outer1 = struct
    (* Left-hand-side can be short-form [alloc] if the mode isn't needed *)
    let f x = x [@exclave_if_stack a'] [@@alloc a' = (heap, stack)]

    (* Right-hand-side can be in long-form [alloc @ mode] *)
    [@@@alloc a' @ m' = (heap_global, a @ m)]

    (* You can pun on an [alloc] *)
    module [@alloc a'] Inner1 = struct
      let f x = x [@exclave_if_stack a']
    end

    (* You can pun on an [alloc @ mode] *)
    module [@alloc a' @ m'] Inner2 = struct
      let f x = x [@exclave_if_stack a']
    end
  end

  (* You can pun on [heap] and [stack] directly. *)
  module [@alloc heap] Outer2 = struct
    let f x = x
  end

  module [@alloc stack] Outer3 = struct
    let f x = x
  end]]

include struct
  module Outer1 = struct
    let f x = x
    and f__stack x = x

    include struct
      module Inner1 = struct
        let f x = x
      end

      module Inner2 = struct
        let f x = x
      end
    end
  end

  module Outer2 = struct
    let f x = x
  end

  module Outer3__stack = struct
    let f x = x
  end
end

include struct
  module Outer1__stack = struct
    let f x = x
    and f__stack x = x

    include struct
      module Inner1 = struct
        let f x = x
      end

      module Inner2 = struct
        let f x = x
      end
    end

    include struct
      module Inner1__stack = struct
        let f x = x
      end

      module Inner2__stack = struct
        let f x = x
      end
    end
  end

  module Outer2 = struct
    let f x = x
  end

  module Outer3__stack = struct
    let f x = x
  end
end

[@@@end]
