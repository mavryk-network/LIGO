type storage is int

[@entry]
function sub (const i : int; const x : storage) : list (operation) * storage is
  ((nil : list (operation)), x - i)

[@entry]
function add (const i : int; const x : storage) : list (operation) * storage is
  ((nil : list (operation)), x + i)