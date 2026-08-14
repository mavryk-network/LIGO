module Euro is {
  type t is int
  function add (const a : t; const b : t) : t is a + b
  const one : t = 1
  const two : t = 2
}