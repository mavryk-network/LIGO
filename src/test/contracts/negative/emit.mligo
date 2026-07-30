[@entry]
let main () (_ : string) : operation list * string =
  let x = "%lol" in
  [Mavryk.emit x 12], x
