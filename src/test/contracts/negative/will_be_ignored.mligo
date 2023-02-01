type storage = {
  owner: address;
}

let main (amount : tez) (s: storage): operation list * storage =
     let receiver : @contract =
      match (Tezos.get_contract_opt (s.owner) : @contract option) with
        Some (@contract) -> @contract
      | None -> (failwith ("Contract not found.") : @contract) in
    let tx : operation = Tezos.transaction unit amount receiver in
    ([tx]: operation list), s
