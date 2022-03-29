type parameter = unit
type storage = int list

let main ((_, storage): (parameter * storage)) = 
    let a = [1; 2;3;4;5;6] in
    ([]: operation list), a 