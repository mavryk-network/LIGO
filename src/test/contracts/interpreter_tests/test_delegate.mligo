

let main (delegate, _ : key_hash option * unit) : operation list * unit =
  let op = Tezos.set_delegate delegate in
  ([op] : operation list), ()

let test =
  let (pk, pkh) = Test.keygen () in
  let () = Test.transfer_to_contract_exn (Tezos.implicit_account pkh : unit contract) () 1mutez in
  let () = Test.register_delegate pkh in

  let (ta, _, _) = Test.originate main () 100mutez in
  let contract = Test.to_contract ta in

  let delegate = Test.delegate contract in
  let () = assert (delegate = (None : key_hash option)) in

  let () = Test.transfer_to_contract_exn contract (Some pkh) 0tez in

  let delegate = Test.delegate contract in
  assert (delegate = Some pkh)
