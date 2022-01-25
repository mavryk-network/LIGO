type a = string
let x = 5
let f (x : int) (y : int) = x + y
let g (x : int) = -x

let main ((_, s) : unit * int) : operation list * int =  
  let h (x : int) (y: int) = x * y in
  let i = h 2 in
  let j (y: int) = i y in
  let s = i (g s) in
  let z = s in 
  let o = z in
  let p = j 5 in
  let s = f s (g s) in
  (([] : operation list), s)