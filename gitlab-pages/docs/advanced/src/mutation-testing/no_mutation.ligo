// This is mutation-contract.ligo
type storage is int

type result is list (operation) * storage

// Two entrypoints
[@entry]
function add (const delta : int; const store : storage) : result is
  block {
    [@no_mutation] const _u = assert (0 = 0)
  } with ((nil : list (operation)), store + delta)

[@entry] [@no_mutation]
function sub (const delta : int; const store : storage) : result is
  ((nil : list (operation)), store - delta)