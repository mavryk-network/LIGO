type storage = int

type ret = operation list * storage


[@entry] let increment (delta : int) (store : storage) : ret =
  [], store + delta

[@entry] let decrement (delta : int) (store : storage) : ret =
  [], store - delta

[@entry] let reset (() : unit) (_ : storage) : ret =
  [], 0
