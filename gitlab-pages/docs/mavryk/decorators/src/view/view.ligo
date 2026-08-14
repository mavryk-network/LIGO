type storage is int

[@view]
function add (const param : int; const s : storage) : int is param + s

[@view]
function get_storage (const _p : int; const s : storage) : int is s

[@entry]
function main (const _u : unit; const s : storage) : list (operation) * storage is
  ((nil : list (operation)), s)