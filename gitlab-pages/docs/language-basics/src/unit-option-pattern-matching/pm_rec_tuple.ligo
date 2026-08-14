type my_record is record [a : int; b : nat; c : string]
type my_tuple is int * nat * string

function on_record (const v : my_record) : int is
  case v of [
    record [ a = a; b = b_renamed; c = _ ] -> a + int (b_renamed)
  ]

function on_tuple (const v : my_tuple) : int is
  case v of [
    (x, y, _) -> x + int (y)
  ]