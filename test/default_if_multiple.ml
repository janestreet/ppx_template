open! Ppx_template_test_common

[@@@disable_unused_warnings]

[@@@expand_inline
  [%%template
  [@@@kind_set.define or_pair = (value, value & value)]
  [@@@kind_set ks = (bits64, or_pair)]
  [@@@kind.default_if_multiple k = ks]

  let id (x : _) = x]]

let id (x : _) = x
let id (x : _) = x
let id__'value_value' (x : _) = x

[@@@end]
