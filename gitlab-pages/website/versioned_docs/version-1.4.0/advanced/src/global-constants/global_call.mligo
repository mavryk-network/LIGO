let c : int -> int = Mavryk.constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2"

[@entry]
let main (_p : unit) (s : int) : operation list * int =
  ([], c s)