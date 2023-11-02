
let main (_p, s : int * int) : operation list * int =
  let  x : int -> nat -> nat -> mav        = (fun _x _y _z -> 1mav) in
  let _y : int -> int -> int -> int -> nat = x in
  //              ^^^    ^^^    ^^^    ^^^
  ([] : operation list), s
