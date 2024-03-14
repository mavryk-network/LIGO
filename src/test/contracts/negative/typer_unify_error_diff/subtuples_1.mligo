
let main (_p : int) (s : int) : operation list * int =
  let  x : int * string * (nat * mav * nat) *          mav           = 1, "a", (1n, 1mav, 1n), 1mav in
  let _y : int *          (nat * mav * int) * string * mav * address = x in
  //             ^^^^^^                ^^^    ^^^^^^         ^^^^^^^
  ([] : operation list), s
