type storage = int * (int -> int)
type parameter = int -> int
type return = operation list * storage

let i = (Tezos.constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2" : int -> int)
let s = (1, i)

let main (f, (k, g) : parameter * storage) : return =
 ([] : operation list), (g k, f)
