let main (_, _ : unit * unit) : operation list * unit =
  let _v = (failwith "foo" : unit) in
  ([] : operation list), ()

let make_call (contr : unit contract) =
  let _ = Test.get_storage_of_address ("KT1RYW6Zm24t3rSquhw1djfcgQeH9gBdsmiL" : address) in
  Test.transfer_to_contract_exn contr () 10mav


let test =
  let (ta, _, _) = Test.originate main () 1mav in
  make_call (Test.to_contract ta)
