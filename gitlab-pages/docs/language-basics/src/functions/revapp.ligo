function f (const x : int) : int is x + 1
function g (const x : int) : int is x - 2
function h (const x : int) : int is x + x - 3

(* Here we apply function f on value 42,
   then apply g on the result,
   and then apply h on the result *)
const result : int = h (g (f (42)))