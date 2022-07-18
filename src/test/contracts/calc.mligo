type parameter = int
type storage = int

let main ((_, storage): (parameter * storage)) = 
    let a = 12 in 
    let b = 14 in 
    let storage = a + 1 in
    ([]: operation list), storage