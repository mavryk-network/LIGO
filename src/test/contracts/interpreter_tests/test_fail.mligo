#import "./contract_under_test/fail_contract.mligo" "C"

let test =
  let orig = Test.originate (contract_of C) () 0mav in
  let contr = Test.to_contract orig.addr in
  let addr = Mavryk.address contr in
  match Test.transfer_to_contract contr (Main ()) 10mav with
  | Success _ -> (failwith "Should fail !" : michelson_program )
  | Fail e -> (
    match e with
    | Rejected x ->
      let (x, addr_fail) = x in
      let () = assert (addr_fail = addr) in
      x
    | _ -> (failwith "Failed, but wrong reason" : michelson_program )
  )
