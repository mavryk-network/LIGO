// PascaLIGO multi-entry counter (Mavryk dialect, 0.73 grammar).
// Exercises [@entry] dispatch, which is attribute-based (no special grammar).

type storage is int

[@entry] function increment (const delta : int; const store : storage) : list(operation) * storage is
  ((nil : list(operation)), store + delta)

[@entry] function decrement (const delta : int; const store : storage) : list(operation) * storage is
  ((nil : list(operation)), store - delta)

[@entry] function reset (const _u : unit; const _store : storage) : list(operation) * storage is
  ((nil : list(operation)), 0)
