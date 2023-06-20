type storage = unit

[@entry] let main (_ : unit) (_ :  storage) : operation list * storage =
  let sr_address = ("sr1R23ax3Gj8NDQFbQRfNnzuKEZhth5qvWVP" : address) in
  let sr_cont = Tezos.get_contract_with_error sr_address "Err" in
  [ Tezos.transaction () 0mutez sr_cont ], ()