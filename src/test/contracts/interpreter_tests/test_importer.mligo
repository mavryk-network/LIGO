#import "test_imported.mligo" "External"

let test =
  let (taddr, _, _) = Test.originate External.main External.D.default.initial 0mav in
  let contr = Test.to_contract taddr in
  let () = assert (Test.get_storage taddr = External.D.default.initial) in
  let _ = Test.transfer_to_contract_exn contr External.D.default.final 0mav in
  assert (Test.get_storage taddr = External.D.default.final)
