type storage is int

[@entry] function left (const i : int; const x : storage) : list (operation) * storage is
  ((nil : list (operation)), x - i)

[@entry] function right (const i : int; const x : storage) : list (operation) * storage is
  ((nil : list (operation)), x + i)