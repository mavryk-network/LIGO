
let main (_p : int) (s : int) : operation list * int =
  let y : string * int * int * string = "foo", 42, 24, "bar" in
  let x : mav    * nat * mav          = y in
  ([] : operation list), s