function compose (const f : int -> int; const g : int -> int; const x : int) : int is
  f (g (x))
const double_incr : int -> int =
  compose ((function (const x : int) : int is x + 1),
           (function (const x : int) : int is 2 * x))  // 2*x + 1
function increment (const x : int) : int is x + 1
function double (const x : int) : int is 2 * x
const double_incr2 : int -> int = compose (increment, double)