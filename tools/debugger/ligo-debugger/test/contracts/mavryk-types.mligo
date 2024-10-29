let empty () : 42 sapling_state = (Mavryk.sapling_empty_state : 42 sapling_state)

[@entry]
let main () (s : int) : operation list * int =
  let s1 = Mavryk.get_balance() in
  let s2 = Mavryk.get_now() in
  let s3 = Mavryk.get_sender() in
  let s4 = empty in
  let () = ignore s1 in
  let () = ignore s2 in
  let () = ignore s3 in
  let () = ignore s4 in
  (([] : operation list), s)
