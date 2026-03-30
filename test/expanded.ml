open! Ppx_template_test_common

[@@@disable_unused_warnings]

[@@@expand_inline
  let x = {| multiple [@@@mode] floating attributes |}

  module%template _ : sig
    val x : string

    [@@@mode.default m1 = (a, b)]

    val x : string

    [@@@mode.default m2 = (c, d)]

    val x : string
  end = struct
    let x = "not duplicated"

    [@@@mode.default m1 = (a, b)]

    let x = "duplicated twice"

    [@@@mode.default m2 = (c, d)]

    let x = "duplicated four times"
  end]

let x = {| multiple [@@@mode] floating attributes |}

module _ : sig
  val x : string

  [@@@ocaml.text "/*"]

  val x__a : string

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  val x__a__c : string

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  val x__a__d : string

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  val x__b : string

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  val x__b__c : string

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  val x__b__d : string

  [@@@ocaml.text "/*"]
end = struct
  let x = "not duplicated"
  let x__a = "duplicated twice"
  let x__a__c = "duplicated four times"
  let x__a__d = "duplicated four times"
  let x__b = "duplicated twice"
  let x__b__c = "duplicated four times"
  let x__b__d = "duplicated four times"
end

[@@@end]

[@@@expand_inline
  let x = {| scoped opens |}

  module%template _ = struct
    module T = struct
      type t = string

      let x = "x"
    end

    module [@mode m = (a, b)] T = struct
      type t = string

      let x = "x"
    end

    module _ : sig
      [@@@mode.default m = (a, b)]

      open T

      val x : string
    end = struct
      [@@@mode.default m = (a, b)]

      open T [@mode m]

      let x = x
    end
  end]

let x = {| scoped opens |}

module _ = struct
  module T = struct
    type t = string

    let x = "x"
  end

  module T__a = struct
    type t = string

    let x = "x"
  end

  module T__b = struct
    type t = string

    let x = "x"
  end

  module _ : sig
    open T

    [@@@ocaml.text "/*"]

    val x__a : string

    [@@@ocaml.text "/*"]

    open T

    [@@@ocaml.text "/*"]

    val x__b : string

    [@@@ocaml.text "/*"]
  end = struct
    open T__a

    let x__a = x

    open T__b

    let x__b = x
  end
end

[@@@end]

[@@@expand_inline
  let x = {| %%template gets inlined |}

  module _ : sig
    [%%template:
    type foo := unit [@@mode m = (global, local)]

    val bar : unit [@@mode m = (global, local)]

    module type Baz := sig end [@@mode m = (global, local)]

    module Quux = Unit [@@mode m = (global, local)]]
  end = struct
    [%%template
    type foo = unit [@@mode m = (global, local)]

    let bar = () [@@mode m = (global, local)]

    module type Baz = sig end [@@mode m = (global, local)]

    module Quux = Unit [@@mode m = (global, local)]]
  end]

let x = {| %%template gets inlined |}

module _ : sig
  type foo := unit
  and foo__local := unit

  val bar : unit

  [@@@ocaml.text "/*"]

  val bar__local : unit

  [@@@ocaml.text "/*"]

  module type Baz := sig end

  [@@@ocaml.text "/*"]

  module type Baz__local := sig end

  [@@@ocaml.text "/*"]

  module Quux = Unit

  [@@@ocaml.text "/*"]

  module Quux__local = Unit

  [@@@ocaml.text "/*"]
end = struct
  type foo = unit
  and foo__local = unit

  let bar = ()
  and bar__local = ()

  module type Baz = sig end
  module type Baz__local = sig end

  module Quux = Unit
  module Quux__local = Unit
end

[@@@end]

[@@@expand_inline
  let x = {| @@deriving attributes are not duplicated |}

  module%template _ : sig
    type foo [@@kind k = (value, immediate)] [@@deriving compare ~localize]
    and bar [@@kind k = (value, immediate)] [@@deriving sexp_of]
  end = struct
    type foo = bool [@@kind k = (value, immediate)] [@@deriving sexp_of]
    and bar = int [@@kind k = (value, immediate)] [@@deriving compare ~localize]
  end]

let x = {| @@deriving attributes are not duplicated |}

module _ : sig
  type foo [@@deriving compare ~localize]
  and foo__immediate [@@immediate]
  and bar [@@deriving sexp_of]
  and bar__immediate [@@immediate]

  include sig
    [@@@ocaml.warning "-32"]

    val compare_foo : foo -> (foo[@merlin.hide]) -> int
    val compare_foo__local : foo -> (foo[@merlin.hide]) -> int
    val compare_foo__immediate : foo__immediate -> (foo__immediate[@merlin.hide]) -> int

    val compare_foo__immediate__local
      :  foo__immediate
      -> (foo__immediate[@merlin.hide])
      -> int

    val compare_bar : bar -> (bar[@merlin.hide]) -> int
    val compare_bar__local : bar -> (bar[@merlin.hide]) -> int
    val compare_bar__immediate : bar__immediate -> (bar__immediate[@merlin.hide]) -> int

    val compare_bar__immediate__local
      :  bar__immediate
      -> (bar__immediate[@merlin.hide])
      -> int

    val sexp_of_foo : foo -> Sexplib0.Sexp.t
    val sexp_of_foo__immediate : foo__immediate -> Sexplib0.Sexp.t
    val sexp_of_bar : bar -> Sexplib0.Sexp.t
    val sexp_of_bar__immediate : bar__immediate -> Sexplib0.Sexp.t
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]
end = struct
  type foo = bool [@@deriving sexp_of]
  and foo__immediate = bool [@@immediate]
  and bar = int [@@deriving compare ~localize]
  and bar__immediate = int [@@immediate]

  include struct
    let _ = fun (_ : foo) -> ()
    let _ = fun (_ : foo__immediate) -> ()
    let _ = fun (_ : bar) -> ()
    let _ = fun (_ : bar__immediate) -> ()

    let sexp_of_foo = (sexp_of_bool : foo -> Sexplib0.Sexp.t)
    and sexp_of_foo__immediate = (sexp_of_bool : foo__immediate -> Sexplib0.Sexp.t)
    and sexp_of_bar = (sexp_of_int : bar -> Sexplib0.Sexp.t)
    and sexp_of_bar__immediate = (sexp_of_int : bar__immediate -> Sexplib0.Sexp.t)

    let _ = sexp_of_foo
    and _ = sexp_of_foo__immediate
    and _ = sexp_of_bar
    and _ = sexp_of_bar__immediate

    let compare_foo__local = (compare_bool__local : foo -> (foo[@merlin.hide]) -> int)

    and compare_foo__immediate__local =
      (compare_bool__local : foo__immediate -> (foo__immediate[@merlin.hide]) -> int)

    and compare_bar__local = (compare_int__local : bar -> (bar[@merlin.hide]) -> int)

    and compare_bar__immediate__local =
      (compare_int__local : bar__immediate -> (bar__immediate[@merlin.hide]) -> int)
    ;;

    let _ = compare_foo__local
    and _ = compare_foo__immediate__local
    and _ = compare_bar__local
    and _ = compare_bar__immediate__local

    let compare_foo =
      (fun a b -> compare_foo__local a b : foo -> (foo[@merlin.hide]) -> int)

    and compare_foo__immediate =
      (fun a b -> compare_foo__immediate__local a b
       : foo__immediate -> (foo__immediate[@merlin.hide]) -> int)

    and compare_bar =
      (fun a b -> compare_bar__local a b : bar -> (bar[@merlin.hide]) -> int)

    and compare_bar__immediate =
      (fun a b -> compare_bar__immediate__local a b
       : bar__immediate -> (bar__immediate[@merlin.hide]) -> int)
    ;;

    let _ = compare_foo
    and _ = compare_foo__immediate
    and _ = compare_bar
    and _ = compare_bar__immediate
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
end

[@@@end]

[@@@expand_inline
  let x = {| zero_alloc_if_local attributes are inserted as appropriate |}

  module%template [@mode m = (global, local)] M : sig
    val foo : unit -> unit [@@zero_alloc_if_local m]
    val bar : unit -> unit [@@zero_alloc_if_local m opt]
    val baz : unit -> unit [@@zero_alloc_if_local m opt arity 1]
  end = struct
    let[@zero_alloc_if_local m] foo () = ()
    let[@zero_alloc_if_local m opt] bar () = ()
    let baz () = (bar [@zero_alloc_if_local m assume]) ()
  end]

let x = {| zero_alloc_if_local attributes are inserted as appropriate |}

module M : sig
  val foo : unit -> unit
  val bar : unit -> unit
  val baz : unit -> unit
end = struct
  let foo () = ()
  let bar () = ()
  let baz () = bar ()
end

module M__local : sig
  val foo : unit -> unit [@@zero_alloc]
  val bar : unit -> unit [@@zero_alloc opt]
  val baz : unit -> unit [@@zero_alloc opt arity 1]
end = struct
  let foo () = () [@@zero_alloc]
  let bar () = () [@@zero_alloc opt]
  let baz () = (bar [@zero_alloc assume]) ()
end

[@@@end]

[@@@expand_inline
  let x = {| [exclave_] is not inserted multiple times |}

  let%template id x = x [@exclave_if_local n] [@exclave_if_stack a]
  [@@alloc a @ m = (heap_global, stack_local)] [@@mode n = (local, global)]
  ;;]

let x = {| [exclave_] is not inserted multiple times |}

let id__local x = x
and id__local__stack x = x
and id x = x
and id__stack x = x

[@@@end]

[@@@expand_inline
  let x = {| name mangling excludes an axis if every element is default |}

  module _ : sig
    val%template foo : unit
    [@@mode
      m = (global, local)
      , n = (nonportable, portable)
      , o = (global, local)
      , p = (nonportable, portable)]
  end = struct
    let%template foo = ()
    [@@mode
      m = (global, local)
      , n = (nonportable, portable)
      , o = (global, local)
      , p = (nonportable, portable)]
    ;;
  end]

let x = {| name mangling excludes an axis if every element is default |}

module _ : sig
  val foo : unit

  [@@@ocaml.text "/*"]

  val foo__nonportable__portable : unit
  val foo__global__local : unit
  val foo__global__local__nonportable__portable : unit
  val foo__portable__nonportable : unit
  val foo__portable__portable : unit
  val foo__global__local__portable__nonportable : unit
  val foo__global__local__portable__portable : unit
  val foo__local__global : unit
  val foo__local__global__nonportable__portable : unit
  val foo__local__local : unit
  val foo__local__local__nonportable__portable : unit
  val foo__local__global__portable__nonportable : unit
  val foo__local__global__portable__portable : unit
  val foo__local__local__portable__nonportable : unit
  val foo__local__local__portable__portable : unit

  [@@@ocaml.text "/*"]
end = struct
  let foo = ()
  and foo__nonportable__portable = ()
  and foo__global__local = ()
  and foo__global__local__nonportable__portable = ()
  and foo__portable__nonportable = ()
  and foo__portable__portable = ()
  and foo__global__local__portable__nonportable = ()
  and foo__global__local__portable__portable = ()
  and foo__local__global = ()
  and foo__local__global__nonportable__portable = ()
  and foo__local__local = ()
  and foo__local__local__nonportable__portable = ()
  and foo__local__global__portable__nonportable = ()
  and foo__local__global__portable__portable = ()
  and foo__local__local__portable__nonportable = ()
  and foo__local__local__portable__portable = ()
end

[@@@end]

let x = {|doc comment hiding for module includes|}

[@@@expand_inline
  module type S = [%template:
  module N : sig
    [@@@kind.default k = (value, bits64, float64)]

    module type O = sig
      type t
    end
  end

  [@@@kind.default k = (value, bits64, float64)]

  module type M = sig
    type t
  end

  include M [@kind k]
  include N.O [@kind k]
  include M [@kind k] with type t := int
  include N.O [@kind k] with type t := int]]

module type S = sig
  module N : sig
    module type O = sig
      type t
    end

    [@@@ocaml.text "/*"]

    module type O__bits64 = sig
      type t
    end

    [@@@ocaml.text "/*"]

    [@@@ocaml.text "/*"]

    module type O__float64 = sig
      type t
    end

    [@@@ocaml.text "/*"]
  end

  module type M = sig
    type t
  end

  include M
  include N.O
  include M with type t := int
  include N.O with type t := int

  [@@@ocaml.text "/*"]

  module type M__bits64 = sig
    type t
  end

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  include M__bits64

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  include N.O__bits64

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  include M__bits64 with type t := int

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  include N.O__bits64 with type t := int

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  module type M__float64 = sig
    type t
  end

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  include M__float64

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  include N.O__float64

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  include M__float64 with type t := int

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  include N.O__float64 with type t := int

  [@@@ocaml.text "/*"]
end

[@@@end]

[@@@expand_inline
  let x = {| wildcard patterns smoke test |}

  module%template _ : sig
    val id : 'a. 'a -> 'a
    [@@kind (f, s) = ((float64, bits64), (bits64, float64))]
    [@@mode l = (global, local), v = (read_write, read)]
  end = struct
    let id x = x
    [@@kind (_, _) = ((float64, bits64), (bits64, float64))]
    [@@mode _ = (global, local), _ = (read_write, read)]
    ;;
  end]

let x = {| wildcard patterns smoke test |}

module _ : sig
  [@@@ocaml.text "/*"]

  val id__float64 : 'a. 'a -> 'a
  val id__float64__read : 'a. 'a -> 'a
  val id__float64__local : 'a. 'a -> 'a
  val id__float64__local__read : 'a. 'a -> 'a
  val id__bits64 : 'a. 'a -> 'a
  val id__bits64__read : 'a. 'a -> 'a
  val id__bits64__local : 'a. 'a -> 'a
  val id__bits64__local__read : 'a. 'a -> 'a

  [@@@ocaml.text "/*"]
end = struct
  let id__float64 x = x
  and id__float64__read x = x
  and id__float64__local x = x
  and id__float64__local__read x = x
  and id__bits64 x = x
  and id__bits64__read x = x
  and id__bits64__local x = x
  and id__bits64__local__read x = x
end

[@@@end]
