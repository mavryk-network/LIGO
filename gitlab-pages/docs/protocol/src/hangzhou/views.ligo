type storage is string

[@entry] function main (const _p : unit; const s : storage) : list (operation) * storage is
  ((nil : list (operation)), s)

// view 'view1', simply returns the storage
[@view] function view1 (const _p : unit; const s : storage) : storage is s

// view 'v2', returns true if the storage has a given length
[@view] function v2 (const expected_length : nat; const s : storage) : bool is
  (String.length (s) = expected_length)

// view 'v3' returns a constant int
[@view] function v3 (const _p : unit; const _s : storage) : int is 42
function view_call (const _name : string; const _param : int; const addr : address) : option (int) is
  Mavryk.call_view ("sto_plus_n", 1, addr)