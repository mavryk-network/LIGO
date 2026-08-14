module Euro is {
  type t is nat
  function add (const a : t; const b : t) : t is a + b
  const one : t = 1n
  const two : t = 2n
}

type storage is Euro.t

function tip (const s : storage) : storage is
  Euro.add (s, Euro.one)