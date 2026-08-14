type storage is int

type ret is list (operation) * storage

// Three entrypoints

[@entry] function increment (const delta : int; const store : storage) : ret is
  ((nil : list (operation)), store + delta)

[@entry] function decrement (const delta : int; const store : storage) : ret is
  ((nil : list (operation)), store - delta)

[@entry] function reset (const _p : unit; const _s : storage) : ret is
  ((nil : list (operation)), 0)
