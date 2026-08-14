(* This is mockup_testme.ligo *)
type storage is string

type result is list (operation) * storage

[@entry]
function append (const s : string; const store : storage) : result is
  ((nil : list (operation)), store ^ s)