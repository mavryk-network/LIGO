type param =
| [@annot foo] A of unit
| B of unit

[@entry]
let main (_ : param) (_ : unit) : operation list * unit =
  let c = (Mavryk.self ("%a") : unit contract) in
  let op = Mavryk.transaction () 0mumav c in
  ([op] : operation list), ()
