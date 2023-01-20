let k1 (type a b) = fun (w : a) -> fun (_x : b) -> w
let k2 (type c d) = fun (y : c) -> fun (_z : d) -> y
let f (b : bool) (str : string) =
  let k (type e f) : e -> f -> e = if b then (k1 : e -> f -> e) else (k2 : e -> f -> e) in
  (k str) (40 + 2)

let v = f true "hello"
