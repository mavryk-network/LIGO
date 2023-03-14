(* IF YOU CHANGE THIS, CHANGE THE EXAMPLE ON THE FRONT PAGE OF THE WEBSITE *)

type storage = int

(* variant defining pseudo multi-entrypoint actions *)

type parameter =
| Increment of int
| Decrement of int

let add (a : int) (b : int) : int = a + b
let sub (a : int) (b : int) : int = a - b

(* real entrypoint that re-routes the flow based on the parameter provided *)

let main (p : parameter) (s : storage) =
 let storage =
   match p with
   | Increment n -> add s n
   | Decrement n -> sub s n
 in ([] : operation list), storage

(* IF YOU CHANGE THIS, CHANGE THE EXAMPLE ON THE FRONT PAGE OF THE WEBSITE *)
