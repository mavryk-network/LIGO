let const = fun (type a b) (a : a) (_ : b) : a -> a

[@entry]
let main (_ : unit) (_ : unit) : operation list * unit = [], const () 0
