
type s       = nat * tez * tez * nat
type s_close = nat * tez * int * nat (* closer to s0 than the others *)
type s1 = string * address * string * tez
type s2 = address * int * int * int

let main (_p : int) (s : int) : operation list * int =
  let  x : int *           s       *      nat = 42, (1n, 1tez, 1tez, 1n), 1n in
  let _y : int * s1 * s2 * s_close * s2 * nat = x in
  ([] : operation list), s
