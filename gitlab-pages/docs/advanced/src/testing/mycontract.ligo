// This is mycontract.ligo
module C is {
  type storage is int
  type result is list (operation) * storage

  // Two entrypoints
  [@entry] function increment (const delta : int; const store : storage) : result is
    ((nil : list (operation)), store + delta)
  [@entry] function decrement (const delta : int; const store : storage) : result is
    ((nil : list (operation)), store - delta)
  [@entry] function reset (const _u : unit; const _store : storage) : result is
    ((nil : list (operation)), 0)
}