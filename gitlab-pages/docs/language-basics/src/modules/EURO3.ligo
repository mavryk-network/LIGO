module EURO is {
  type t is nat

  function add (const a : t; const b : t) : t is a + b

  module CONST is {
    const zero : t = 0n
    const one : t = 1n
  }
}
type storage is EURO.t

[@entry] function main (const _action : unit; const store : storage) : list (operation) * storage is
  ((nil : list (operation)), EURO.add (store, EURO.CONST.one))