let main (_,_ : unit * string ) : operation list * string =
  [Mavryk.emit "%hello world" 12], "bye"
