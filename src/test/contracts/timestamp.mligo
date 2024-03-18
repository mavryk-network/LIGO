type storage = timestamp

let main (p : unit) (s : storage) : operation list * storage =
  [], Mavryk.get_now ()
