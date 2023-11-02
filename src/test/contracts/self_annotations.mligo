type param =
  | [@annot foo] A of unit
  | B of unit

let main (_,_ : param * unit) : operation list * unit =
  let c = (Mavryk.self("%foo") : unit contract) in
  let op = Mavryk.transaction () 0mumav c in
  ([op] : operation list), ()
