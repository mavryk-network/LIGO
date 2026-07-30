type parameter =
| Increment of int
| Extend of never

type storage = int

[@entry]
let main (action : parameter) (store : storage) : operation list * storage =
  (([] : operation list),
   (match action with
      Increment (n) -> store + n
    | Extend (k) -> (Mavryk.never k : storage)))
