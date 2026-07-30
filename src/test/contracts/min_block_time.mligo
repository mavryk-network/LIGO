[@entry]
let main (_ : unit) (_ : nat) : operation list * nat =
  ([], Mavryk.get_min_block_time ())
