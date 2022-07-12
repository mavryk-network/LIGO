type parameter = unit
type storage = int set

let main ((_, storage): (parameter * storage)) = 
    let a = Set.add 1 Set.empty in
    ([]: operation list), a 