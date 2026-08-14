type colour is
  | RGB of int * int * int
  | Gray of int
  | Default of unit

function int_of_colour (const c : colour) : int is
  case c of [
    RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray (i)    -> 232 + i
  | Default (_u) -> 0
  ]