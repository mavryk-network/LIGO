type storage is record [ fn : option (int -> int); value : int ]
type result is list (operation) * storage

function call (const fn : option (int -> int); const value : int) : int is
  case fn of [
    Some (f) -> f (value)
  | None -> (failwith ("Lambda is not set") : int)
  ]

[@entry]
function setFunction (const fn : int -> int; const s : storage) : result is
  ((nil : list (operation)), s with record [ fn = Some (fn) ])

[@entry]
function callFunction (const _u : unit; const s : storage) : result is
  ((nil : list (operation)), s with record [ value = call (s.fn, s.value) ])