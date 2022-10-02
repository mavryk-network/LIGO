type storage = int * (int -> int)
type parameter = unit
type return = operation list * storage

let y = 42
let f = fun (x : int) -> x + y

let k = (5, (fun (_ : unit) -> (Tezos.constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2" : int -> int)) ())
let j = (y, f)

let main ((), store : parameter * storage) : return =
 ([] : operation list), store
