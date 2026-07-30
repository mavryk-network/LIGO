type storage = address
type return = operation list * storage
type parameter = One of address | Two

let main (_, _ : parameter * storage) : operation list * storage =
  let c : parameter contract = Mavryk.self "%default" in
  ( ([] : operation list), Mavryk.address c )
