type storage is record [ storage : int; dynamic_entrypoints ]

[@dyn_entry]
function one (const _u : unit; const _i : int) : list (operation) * int is
  ((nil : list (operation)), 1)

[@dyn_entry]
function tick (const _t : ticket (int); const p : int * int) : list (operation) * (int * int) is
  ((nil : list (operation)), p)