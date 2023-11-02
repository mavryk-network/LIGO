type paramater = unit
type storage = int
type _return = operation list * storage * mav

let main (_,s : paramater * storage) : _return =
    [], s, 1mav