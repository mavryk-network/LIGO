type storage = {
  owner: address;
}

let main (amoun, s: mav * storage): operation list * storage =
     let receiver : contract =
      match (Mavryk.get_contract_opt(s.owner) : contract option) with
        Some (contract) -> contract
      | None -> (failwith ("Contract not found.") : contract) in
    let tx : operation = Mavryk.transaction unit amoun receiver in
    ([tx]: operation list), s
