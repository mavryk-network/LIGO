let empty () = (Tezos.sapling_empty_state : 42 sapling_state)

let main (_, s : unit * int) : operation list * int =
  let s4 = empty in
  let () = ignore s4 in
(*  let () = ignore s5 in *)
  (([] : operation list), s)
