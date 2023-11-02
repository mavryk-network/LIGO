let main (_, _ : unit * unit) : operation list * unit =
  ([] : operation list), ()

let test =
  let (taddr, _, _) = Test.originate main () 0mav in
  let contr = Test.to_contract taddr in
  let addr = Mavryk.address contr in
  let () = Test.log addr in
  let () = Test.set_source addr in
  let _ = Test.transfer_exn addr (Test.eval ()) 0mav in
  ()
