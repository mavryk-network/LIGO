type storage is int
type return_type is list (operation) * storage

[@entry] function add (const n : int; const storage : storage) : return_type is
  ((nil : list (operation)), storage + n)
[@entry] function sub (const n : int; const storage : storage) : return_type is
  ((nil : list (operation)), storage - n)