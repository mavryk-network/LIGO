type storage is int

type parameter is
  | Sub of int
  | Add of int

[@entry]
function main (const p : parameter; const x : storage) : list (operation) * storage is
  ((nil : list (operation)),
   case p of [
     Sub (i) -> x - i
   | Add (i) -> x + i
   ])