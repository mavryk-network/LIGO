// This is mutation-contract.ligo
module C is {
  type storage is int

  // Two entrypoints
  [@entry] function add (const delta : int; const store : storage) : list (operation) * storage is
    ((nil : list (operation)), store + delta)
  [@entry] function sub (const delta : int; const store : storage) : list (operation) * storage is
    ((nil : list (operation)), store - delta)
}