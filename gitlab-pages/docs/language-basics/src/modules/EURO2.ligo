module EURO is {
  type t is int
  function add (const a : t; const b : t) : t is a + b
  const zero : t = 0
  const one : t = 1
}