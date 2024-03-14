#import "C.mligo" "C"

let test =
  let orig = Test.originate (contract_of C) () 0mav in
  let _ = Test.transfer_exn orig.addr (Main ()) 0mav in
  assert (Test.get_storage orig.addr = ())
