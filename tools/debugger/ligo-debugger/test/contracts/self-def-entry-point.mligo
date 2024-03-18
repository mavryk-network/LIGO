[@entry]
let main () () : operation list * unit =
  let c = (Mavryk.self("%default") : unit contract) in
  let op = Mavryk.transaction () 0mumav c in
  (([op] : operation list), ())
