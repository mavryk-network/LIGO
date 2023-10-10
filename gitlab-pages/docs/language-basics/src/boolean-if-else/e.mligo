let a : tez  = 5mutez
let b : tez  = 10mutez
let c : bool = (a = b) // false
type magnitude = Small | Large (* See variant types. *)

let compare (n : nat) : magnitude =
  if n < 10n then Small else Large