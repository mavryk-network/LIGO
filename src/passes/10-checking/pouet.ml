---- < ASTCORE
type t = @layout tree { a : int ; b : nat }
let f = fun x -> let type u = @layout comb { c: int ; d : nat} in x 

--- >ASTCORE

type t = { a : int ; b : nat }
let f = fun x -> let type u = { c: int ; d : nat} in x 