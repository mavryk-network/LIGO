type storage = string * nat * string * nat * string

type parameter = int
type return = operation list * storage

let main (_, store : parameter * storage) : return =
 [], store

[@view] let v (_ : int * storage) : mav = 1mav