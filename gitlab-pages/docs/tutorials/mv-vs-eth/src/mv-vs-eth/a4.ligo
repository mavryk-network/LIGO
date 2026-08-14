type storage is int
type result is list (operation) * storage

[@entry]
function add (const i : int; const s : storage) : result is
  ((nil : list (operation)), s + i)

[@entry]
function subtract (const i : int; const s : storage) : result is
  ((nil : list (operation)), s - i)