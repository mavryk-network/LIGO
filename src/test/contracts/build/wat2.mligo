#import "./wat.mligo" "Wat"

let bar = Wat.bar
let foo = Wat.foo


let main (((),_) : unit * int) : operation list * int = [], foo
