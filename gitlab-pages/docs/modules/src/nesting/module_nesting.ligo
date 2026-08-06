module Euro is {
  type t is nat

  function add (const a : t; const b : t) : t is a + b

  module Coin is {
    const one : t = 1n
    const two : t = 2n
  }
}
type storage is Euro.t

function increment (const s : storage) : storage is
  Euro.add (s, Euro.Coin.one)