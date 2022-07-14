type parameter = int
type storage = int

let main ((_,_):(parameter * storage)) =
  let a = 42 in 
  let result = a + 1 in 
  ([]: operation list), result
