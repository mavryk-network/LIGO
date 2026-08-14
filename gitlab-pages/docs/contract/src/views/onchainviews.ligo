type storage is string
type ret is list (operation) * storage

[@entry]
function main (const word : string; const store : storage) : ret is
  ((nil : list (operation)), store ^ " " ^ word)

// view 'view1', simply returns the storage
[@view] function view1 (const _u : unit; const s : storage) : storage is
  s

// view 'v2', returns true if the storage has a given length
[@view] function v2 (const expected_length : nat; const s : storage) : bool is
  (String.length (s) = expected_length)

// view 'v3' does not use its parameters and returns a constant int
[@view] function v3 (const _u : unit; const _s : storage) : int is
  42