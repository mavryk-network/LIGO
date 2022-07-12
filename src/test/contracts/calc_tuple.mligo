type parameter = unit
type storage = int * int * int * int

let x = 3
let y = 20

let foo (a: int) (b: int) = a + b

let main ((_, storage): (parameter * storage)) = 
    (* let (a, b, c, d) = storage in *)
    ([]: operation list), storage
    
    (* (a + 5, b - 5, c * 5, d / 5) *)