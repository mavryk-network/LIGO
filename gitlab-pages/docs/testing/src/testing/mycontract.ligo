// This is mycontract.ligo
module MyContract is {
  type storage is int
  type result is list (operation) * storage

  [@entry] function increment (const delta : int; const storage : storage) : result is
    ((nil : list (operation)), storage + delta)
  [@entry] function decrement (const delta : int; const storage : storage) : result is
    ((nil : list (operation)), storage - delta)
  [@entry] function reset (const _u : unit; const _storage : storage) : result is
    ((nil : list (operation)), 0)
}