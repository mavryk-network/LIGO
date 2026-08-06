module IncDec is {
  type storage is int
  type result is list (operation) * storage

  // Four entrypoints

  [@entry] function increment (const delta : int; const store : storage) : result is
    ((nil : list (operation)), store + delta)

  [@entry] function default (const _u : unit; const store : storage) : result is
    increment (1, store)

  [@entry] function decrement (const delta : int; const store : storage) : result is
    ((nil : list (operation)), store - delta)

  [@entry] function reset (const _p : unit; const _s : storage) : result is
    ((nil : list (operation)), 0)
}