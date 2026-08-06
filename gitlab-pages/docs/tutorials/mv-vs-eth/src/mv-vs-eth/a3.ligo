type storage is int
type result is list (operation) * storage

[@entry]
function increment (const _u : unit; const s : storage) : result is
  ((nil : list (operation)), s + 1)

[@entry]
function decrement (const _u : unit; const s : storage) : result is
  ((nil : list (operation)), s - 1)