let add (a, b : int * int) : int = a + b

[@entry]
let main (_, s : unit * int) : operation list * int =
  (([] : operation list), add(s, s + 2))
