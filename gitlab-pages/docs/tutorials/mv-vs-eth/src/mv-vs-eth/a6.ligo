type storage is int

[@entry]
function compute (const func : int -> int; const s : storage) : list (operation) * storage is
  ((nil : list (operation)), func (s))