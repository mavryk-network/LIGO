let michelson_add : int * int -> int =
  [%Michelson ({| { UNPAIR ; ADD } |} : int * int -> int) ]

let main (x, s : int * int) : operation list * int =
  ([] : operation list), michelson_add (x, s)

let test =
  let (taddr, _, _) = Test.originate main 1 0mav in
  let c = Test.to_contract taddr in
  let _ = Test.transfer_to_contract_exn c 41 0mav in
  Test.log (Test.get_storage taddr)
