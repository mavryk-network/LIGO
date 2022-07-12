type parameter = unit
type storage = int

let x = 3
let y = 20

//let foo (a: int) = 
//    let c = 12 in
//   fun (b: int) -> a + b + c

let foo (a: int) (c: int) = a + c

let f (a:int) = ()

let main ((_, storage): (parameter * storage)) = 
    (* let _ = List.iter f [1; 2; 3] in *)
    ([]: operation list), storage