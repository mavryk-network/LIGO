
type parameter =
  Increment of int
| Extend of never

type storage = int

let main(action, store : parameter * storage) : operation list * storage =
  (([] : operation list),
   (match action with
      Increment (n) -> store + n
    | Extend (k) -> (Mavryk.never k : storage))
  )
