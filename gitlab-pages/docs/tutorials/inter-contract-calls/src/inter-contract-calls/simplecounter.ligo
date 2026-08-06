(* examples/contracts/ligo/SimpleCounter.ligo *)

[@entry]
function main (const param : int; const storage : int) : list (operation) * int is
  ((nil : list (operation)), param + storage)