
type parameter = (nat * int * bool list)
// type parameter = int
type storage = bool

let main (_actions, _storage : parameter * storage) : operation list * storage =
  ([] : operation list), true