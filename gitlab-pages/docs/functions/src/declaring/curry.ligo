function add (const x : int; const y : int) : int is x + y  // Uncurried

function add_curry (const x : int) : int -> int is
  function (const y : int) : int is x + y  // Curried

const increment : int -> int = add_curry (1)  // Partial application
const one : int = increment (0)