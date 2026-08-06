function add (const ab : int * int) : int is ab.0 + ab.1              // Uncurried
function add_curry (const a : int; const b : int) : int is a + b      // Curried
const increment : int -> int = add_curry (1)                          // Partial application