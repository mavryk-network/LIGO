[@entry]
let main (p : int * int) (_ : unit) : operation list * unit =
  [Mavryk.emit "%foo" p; Mavryk.emit "%bar" p.0], ()
