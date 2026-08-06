function sum (const x : int; const y : int) : int is x + y

function main (const parameter : int; const storage : int) : list (operation) * int is
  ((nil : list (operation)), sum (parameter, storage))
const n : int = 4

function main (const _p : unit; const _s : int) : list (operation) * int is
  ((nil : list (operation)), n * n)
type storage is record [ large_entrypoint : big_map (bool, int -> int); result : int ]

function load_large_ep (const store : storage) : int -> int is
  case Big_map.find_opt (True, store.large_entrypoint) of [
    Some (ep) -> ep
  | None -> (failwith ("Internal error") : int -> int)
  ]

[@entry] function large_entry_point (const n : int; const store : storage) : list (operation) * storage is
  ((nil : list (operation)), store with record [ result = (load_large_ep (store)) (n) ])

(* Other entrypoints ... *)