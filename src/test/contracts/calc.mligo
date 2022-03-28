type parameter = unit
type storage = int

let x = 3
let y = 20

//let foo (a: int) = 
//    let c = 12 in
//   fun (b: int) -> a + b + c

let foo (a: int) (c: int) = a + c

let main ((_, storage): (parameter * storage)) = 
    let a = (1, 2, 3, storage) in
    ([]: operation list), foo a.3 (x + y)