let test_new =
  let (_sk, pk) = Test.new_account () in
  let pkh = Crypto.hash_key pk in
  let c = Mavryk.implicit_account pkh in
  let a = Mavryk.address c in
  let to_ = Test.nth_bootstrap_account 0 in
  let _ = Test.transfer_exn a (Test.eval ()) 123mav in
  let _ = Test.set_source a in
  let _ = Test.transfer_exn to_ (Test.eval ()) 12mav in
  Test.get_balance a

let test_add =
  let () = Test.reset_state 2n ([] : mav list) in
  let sk = "edsk3FhQ1djEDDCfqseyfbrpwkw5ogTDAaryXAdQGhk5Vpw6VGgo6v" in
  let pk = ("edpkv2kByfiJmUHr3SCp2rpASF2xSEhT248MSNEAZK9ho86sMBdcuE" : key) in
  let () = Test.add_account sk pk in
  let pkh = Crypto.hash_key pk in
  let c = Mavryk.implicit_account pkh in
  let a = Mavryk.address c in
  let to_ = Test.nth_bootstrap_account 0 in
  let _ = Test.transfer_exn a (Test.eval ()) 123mav in
  let _ = Test.set_source a in
  let _ = Test.transfer_exn to_ (Test.eval ()) 12mav in
  Test.get_balance a
