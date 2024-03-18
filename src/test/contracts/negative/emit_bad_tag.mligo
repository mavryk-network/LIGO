[@entry]
let main (_ : unit) (_ : string) : operation list * string =
  [Mavryk.emit "%hello world" 12], "bye"
