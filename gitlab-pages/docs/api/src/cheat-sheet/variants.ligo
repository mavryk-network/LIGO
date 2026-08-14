type action is
  Increment of int
| Decrement of int
const a : action = Increment (5)

const result : int =
  case a of [
    Increment (n) -> n + 1
  | Decrement (n) -> n - 1
  ]