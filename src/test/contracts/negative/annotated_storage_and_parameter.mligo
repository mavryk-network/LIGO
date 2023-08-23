type storage = (int, int) map

type 'a parameter = 'a list

[@entry]
let main (_p : parameter) (s : storage) : operation list * storage = ([], s)
