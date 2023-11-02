let main (p,_ : (int*int) * unit ) : operation list * unit =
  [Mavryk.emit "%foo" p ; Mavryk.emit "%bar" p.0],()