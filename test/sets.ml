[@@@disable_unused_warnings]

(* basic sets in the RHS of bindings *)

[@@@expand_inline
  module%template M : sig
    val id : 'a -> 'a [@@kind k = (base, (value, bits64) & base_or_null)]
  end = struct
    let id x = x [@@kind k = (base, (value, bits64) & base_or_null)]
  end

  let%template id = (M.id [@kind k]) [@@kind k = (base, (value, bits64) & base_or_null)]

  [%%template
  [@@@kind_set.define values = (value, value_or_null)]
  [@@@kind.default k1 = (value, values)]
  [@@@kind.default k2 = (values mod contended, bits64)]

  let const2 _ _ x = x]]

module M : sig
  [@@@ocaml.text "/*"]

  val id__bits64 : 'a -> 'a
  val id__bits32 : 'a -> 'a
  val id__word : 'a -> 'a
  val id__float64 : 'a -> 'a
  val id__float32 : 'a -> 'a

  [@@@ocaml.text "/*"]

  val id : 'a -> 'a

  [@@@ocaml.text "/*"]

  val id__'value_bits64' : 'a -> 'a
  val id__'value_bits32' : 'a -> 'a
  val id__'value_word' : 'a -> 'a
  val id__'value_float64' : 'a -> 'a
  val id__'value_float32' : 'a -> 'a
  val id__'value_value_or_null' : 'a -> 'a
  val id__'bits64_bits64' : 'a -> 'a
  val id__'bits64_bits32' : 'a -> 'a
  val id__'bits64_word' : 'a -> 'a
  val id__'bits64_float64' : 'a -> 'a
  val id__'bits64_float32' : 'a -> 'a
  val id__'bits64_value_or_null' : 'a -> 'a

  [@@@ocaml.text "/*"]
end = struct
  let id__bits64 x = x
  and id__bits32 x = x
  and id__word x = x
  and id__float64 x = x
  and id__float32 x = x
  and id x = x
  and id__'value_bits64' x = x
  and id__'value_bits32' x = x
  and id__'value_word' x = x
  and id__'value_float64' x = x
  and id__'value_float32' x = x
  and id__'value_value_or_null' x = x
  and id__'bits64_bits64' x = x
  and id__'bits64_bits32' x = x
  and id__'bits64_word' x = x
  and id__'bits64_float64' x = x
  and id__'bits64_float32' x = x
  and id__'bits64_value_or_null' x = x
end

let id__bits64 = M.id__bits64
and id__bits32 = M.id__bits32
and id__word = M.id__word
and id__float64 = M.id__float64
and id__float32 = M.id__float32
and id = M.id
and id__'value_bits64' = M.id__'value_bits64'
and id__'value_bits32' = M.id__'value_bits32'
and id__'value_word' = M.id__'value_word'
and id__'value_float64' = M.id__'value_float64'
and id__'value_float32' = M.id__'value_float32'
and id__'value_value_or_null' = M.id__'value_value_or_null'
and id__'bits64_bits64' = M.id__'bits64_bits64'
and id__'bits64_bits32' = M.id__'bits64_bits32'
and id__'bits64_word' = M.id__'bits64_word'
and id__'bits64_float64' = M.id__'bits64_float64'
and id__'bits64_float32' = M.id__'bits64_float32'
and id__'bits64_value_or_null' = M.id__'bits64_value_or_null'

let const2__value__'value_mod_contended' _ _ x = x
let const2__value__'value_or_null_mod_contended' _ _ x = x
let const2__value__bits64 _ _ x = x
let const2__value_or_null__'value_mod_contended' _ _ x = x
let const2__value_or_null__'value_or_null_mod_contended' _ _ x = x
let const2__value_or_null__bits64 _ _ x = x

[@@@end]

(* The [[@kind_set]] attribute *)

[@@@expand_inline
  [%%template
  [@@@kind_set.define mini_base = (value, bits64, bits32)]

  module type S = sig
    val const : 'b -> 'a -> 'b [@@kind k = ks]
    val add : 'a -> 'a -> 'a [@@kind k = (bits64, ks & value)]
  end
  [@@kind_set ks = (value, mini_base)]

  [@@@kind_set.default ks1 = (value, mini_base)]
  [@@@kind_set.default ks2 = (value, mini_base)]

  module type S2 = sig
    include S [@kind_set ks1]

    val const2 : 'c -> 'a -> 'b -> 'c [@@kind k1 = ks1, k2 = ks2]
  end]]

module type S = sig
  val const : 'b -> 'a -> 'b

  [@@@ocaml.text "/*"]

  val add__bits64 : 'a -> 'a -> 'a
  val add__'value_value' : 'a -> 'a -> 'a

  [@@@ocaml.text "/*"]
end

module type S__''mini_base'' = sig
  val const : 'b -> 'a -> 'b

  [@@@ocaml.text "/*"]

  val const__bits64 : 'b -> 'a -> 'b
  val const__bits32 : 'b -> 'a -> 'b

  [@@@ocaml.text "/*"]

  [@@@ocaml.text "/*"]

  val add__bits64 : 'a -> 'a -> 'a
  val add__'value_value' : 'a -> 'a -> 'a
  val add__'bits64_value' : 'a -> 'a -> 'a
  val add__'bits32_value' : 'a -> 'a -> 'a

  [@@@ocaml.text "/*"]
end

module type S2 = sig
  include S

  val const2 : 'c -> 'a -> 'b -> 'c
end

module type S2__''value''__''mini_base'' = sig
  include S

  val const2 : 'c -> 'a -> 'b -> 'c

  [@@@ocaml.text "/*"]

  val const2__value__bits64 : 'c -> 'a -> 'b -> 'c
  val const2__value__bits32 : 'c -> 'a -> 'b -> 'c

  [@@@ocaml.text "/*"]
end

module type S2__''mini_base''__''value'' = sig
  include S__''mini_base''

  val const2 : 'c -> 'a -> 'b -> 'c

  [@@@ocaml.text "/*"]

  val const2__bits64__value : 'c -> 'a -> 'b -> 'c
  val const2__bits32__value : 'c -> 'a -> 'b -> 'c

  [@@@ocaml.text "/*"]
end

module type S2__''mini_base''__''mini_base'' = sig
  include S__''mini_base''

  val const2 : 'c -> 'a -> 'b -> 'c

  [@@@ocaml.text "/*"]

  val const2__value__bits64 : 'c -> 'a -> 'b -> 'c
  val const2__value__bits32 : 'c -> 'a -> 'b -> 'c
  val const2__bits64__value : 'c -> 'a -> 'b -> 'c
  val const2__bits64__bits64 : 'c -> 'a -> 'b -> 'c
  val const2__bits64__bits32 : 'c -> 'a -> 'b -> 'c
  val const2__bits32__value : 'c -> 'a -> 'b -> 'c
  val const2__bits32__bits64 : 'c -> 'a -> 'b -> 'c
  val const2__bits32__bits32 : 'c -> 'a -> 'b -> 'c

  [@@@ocaml.text "/*"]
end

[@@@end]

(* mangle more interesting kind set atoms *)
[@@@expand_inline
  [%%template
    module [@kind_set ks = (bits64, base_or_null mod separable)] Array = struct
      [@@@kind.default k = ks]

      type 'a t = 'a array

      let singleton (x : _) = [| x |]
    end]

  let arr =
    let open Array [@kind_set base_or_null mod separable] in
    (singleton [@kind bits64 mod separable]) 0L
  ;;]

module Array__''bits64'' = struct
  type 'a t__bits64 = 'a array

  let singleton__bits64 (x : _) = [| x |]
end

module Array__'''base_or_null_mod_separable''' = struct
  type 'a t__'bits64_mod_separable' = 'a array

  let singleton__'bits64_mod_separable' (x : _) = [| x |]

  type 'a t__'bits32_mod_separable' = 'a array

  let singleton__'bits32_mod_separable' (x : _) = [| x |]

  type 'a t__'word_mod_separable' = 'a array

  let singleton__'word_mod_separable' (x : _) = [| x |]

  type 'a t__'float64_mod_separable' = 'a array

  let singleton__'float64_mod_separable' (x : _) = [| x |]

  type 'a t__'float32_mod_separable' = 'a array

  let singleton__'float32_mod_separable' (x : _) = [| x |]

  type 'a t__'value_or_null_mod_separable' = 'a array

  let singleton__'value_or_null_mod_separable' (x : _) = [| x |]
end

let arr =
  let open Array__'''base_or_null_mod_separable''' in
  singleton__'bits64_mod_separable' 0L
;;

[@@@end]

(* separate mangling for a set and its constituents avoids ambiguity *)

[@@@expand_inline
  [%%template
  let x = () [@@kind k = bits64] [@@kind_set ks = base]
  let x = () [@@kind_set ks = base] [@@kind k = bits64]]]

let x__''base''__bits64 = ()
let x__''base''__bits64 = ()

[@@@end]

(* referencing existing sets in [[@@@kind_set.define]] *)
[%%template
[@@@kind_set.define ints = (bits64, bits32, word)]
[@@@kind_set.define floats = (float64, float32)]
[@@@kind_set.define my_base = (value, ints, floats)]

(* show that our base matches [base] *)

module [@kind_set ks = (my_base, base)] M = struct
  [@@@kind.default k = ks]

  let id (x : _) = x
end

module _ : sig
  include module type of struct
    include M [@kind_set my_base]
  end
end =
  M
  [@kind_set base]

module _ : sig
  include module type of struct
    include M [@kind_set base]
  end
end =
  M
  [@kind_set my_base]

(* now add pairs *)

[@@@kind_set.define my_all = (my_base, my_base & my_base)]
[@@@kind_set.define all = (base, base & base)]

module [@kind_set ks = (my_all, all)] M_all = struct
  [@@@kind.default k = ks]

  let id (x : _) = x
end

module _ : sig
  include module type of struct
    include M_all [@kind_set my_all]
  end
end =
  M_all
  [@kind_set all]

module _ : sig
  include module type of struct
    include M_all [@kind_set all]
  end
end =
  M_all
  [@kind_set my_all]]

(* we can still reference modules even after the kind set we added has left scope *)
let id =
  let module M = M_all [@kind_set my_all] in
  M.id [@kind bits64 & value]
;;
