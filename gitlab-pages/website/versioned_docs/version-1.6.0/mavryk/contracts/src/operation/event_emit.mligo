type storage = unit

[@entry]
let main (param : int * int) () : operation list * storage =
  [Mavryk.emit "%foo" param; Mavryk.emit "%bar" param.0], ()