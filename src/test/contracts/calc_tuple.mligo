type parameter = unit
type storage = int * int * int * int

let x = 3
let y = 20

let foo (a: int) (b: int) = a + b

let main ((_, storage): (parameter * storage)) = 
    let (a, b, c, d) = storage in
    ([]: operation list), (foo a (x + y), foo b (x + 10), foo b (x + 20), c + 20)