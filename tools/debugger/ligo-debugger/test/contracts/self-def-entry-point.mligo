[@entry]
let main () () : operation list * unit =
  let c = (Tezos.self("%default") : unit contract) in
  let op = Tezos.transaction () 0mumav c in
  (([op] : operation list), ())
