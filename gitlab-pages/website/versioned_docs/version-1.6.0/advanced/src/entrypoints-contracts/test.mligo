#import "gitlab-pages/docs/advanced/src/entrypoints-contracts/incdec.mligo" "C"

let test =
  let {addr ; code = _ ; size = _} = Test.originate (contract_of C.IncDec) 0 (0mav) in
  let _ = Test.transfer_exn addr (Increment 42) (0mav) in
  assert (42 = Test.get_storage(addr))