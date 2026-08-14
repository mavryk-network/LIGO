const sum = function (const x : int; const y : int) : int is x + y  // Uncurried
const add = function (const (x, y) : int * int) : int is x + y      // Curried
const increment = function (const x : int) : int is x + 1