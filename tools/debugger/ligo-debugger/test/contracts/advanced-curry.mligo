let apply (f : int -> int -> int -> int -> int) (a, b, c, d : int * int * int * int) : int =
  let applyOnce = f a in
  let applyTwice = applyOnce b in
  let applyTwiceDuplicated = applyTwice in
  let applyThrice = applyTwiceDuplicated c in
  let result = applyThrice d + 1 in
  result

let act (a : int) (b : int) (c : int) (d : int) = a + b + c + d

let main (_, s : unit * int) : operation list * int =
  (([] : operation list), apply act (s, s + 2, s + 3, s + 4))
