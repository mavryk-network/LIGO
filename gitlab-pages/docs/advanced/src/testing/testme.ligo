// This is testme.ligo

type storage is int
type result is list (operation) * storage

[@entry]
function increment (const delta : int; const store : storage) : result is
  ((nil : list (operation)), store + delta)

[@entry]
function decrement (const delta : int; const store : storage) : result is
  ((nil : list (operation)), store - delta)

[@entry]
function reset (const _u : unit; const _s : storage) : result is
  ((nil : list (operation)), 0)