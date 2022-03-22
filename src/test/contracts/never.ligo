
type parameter is
  Increment of int
| Extend of never

type storage is int

function main(const (action, store) : parameter * storage) : list (operation) * storage is
  ((nil : list (operation)),
   case action of [
     Increment (n) -> store + n
   | Extend (k) -> (Tezos.never(k) : storage)
   ])
