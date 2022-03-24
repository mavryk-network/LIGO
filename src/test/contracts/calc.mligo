type parameter = unit
type storage = int

let x = 3
let y = 20

let foo (a: int) (b: int) = a + b

let main ((_, storage): (parameter * storage)) = 
    let a = storage in
    ([]: operation list), foo a (x + y)