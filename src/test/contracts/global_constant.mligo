type storage = int

type parameter = unit

type return = operation list * storage

let v =
  (Mavryk.constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2"
   : int -> int)
    42

[@entry]
let main (() : parameter) (store : storage) : return =
  ([] : operation list),
  ((Mavryk.constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2"
    : int -> int)
     store)
