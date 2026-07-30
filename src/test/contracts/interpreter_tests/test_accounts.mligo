module Test = Test.Next

let cast_implicit_account c : implicit_address =
  Test.Address.to_typed_address c

let test_new =
  let {addr=_; pk; sk=_} = Test.Account.new () in
  let pkh = Crypto.hash_key pk in
  let c = Mavryk.implicit_account pkh in
  let a = Mavryk.address c in
  let to_ = Test.Account.address 0 in
  let _ = Test.Contract.transfer_exn c () 123mav in
  let _ = Test.State.set_source a in
  let _ = Test.Typed_address.transfer_exn (cast_implicit_account to_) () 12mav in
  Test.Address.get_balance a

let test_add =
  let () = Test.State.reset 2n ([] : mav list) in
  let sk = "edsk3FhQ1djEDDCfqseyfbrpwkw5ogTDAaryXAdQGhk5Vpw6VGgo6v" in
  let pk = ("edpkv2kByfiJmUHr3SCp2rpASF2xSEhT248MSNEAZK9ho86sMBdcuE" : key) in
  let () = Test.Account.add sk pk in
  let pkh = Crypto.hash_key pk in
  let c = Mavryk.implicit_account pkh in
  let a = Mavryk.address c in
  let to_ = Test.Account.address 0 in
  let _ = Test.Contract.transfer_exn c () 123mav in
  let _ = Test.State.set_source a in
  let _ = Test.Typed_address.transfer_exn (cast_implicit_account to_) () 12mav in
  Test.Address.get_balance a

let cast_implicit_account c : implicit_address = Test.Address.to_typed_address c

let test_new =
  let () = Test.State.reset 2n ([] : mav list) in
  let i = Test.Account.new () in
  let () = Test.Assert.assert (i.addr = Mavryk.address (Mavryk.implicit_account (Crypto.hash_key i.pk))) in
  let _ = Test.transfer_exn (cast_implicit_account i.addr) () 123mav in
  let _ = Test.State.set_source i.addr in
  let to_ = Test.Account.alice () in
  let _ = Test.transfer_exn (cast_implicit_account to_) () 12mav in
  Test.Address.get_balance i.addr

let test_add =
  let () = Test.State.reset 2n ([] : mav list) in
  let pk = ("edpkv2kByfiJmUHr3SCp2rpASF2xSEhT248MSNEAZK9ho86sMBdcuE" : key) in
  let i : Test.Account.info =
    { addr = Mavryk.address (Mavryk.implicit_account (Crypto.hash_key pk))
    ; sk = "edsk3FhQ1djEDDCfqseyfbrpwkw5ogTDAaryXAdQGhk5Vpw6VGgo6v"
    ; pk
    }
  in
  let () = Test.Account.add i.sk i.pk in
  let _ = Test.transfer_exn (cast_implicit_account i.addr) () 123mav in
  let _ = Test.State.set_source i.addr in
  let to_ = Test.Account.alice () in
  let _ = Test.transfer_exn (cast_implicit_account to_) () 12mav in
  Test.Address.get_balance i.addr
