let ownerAddress : address = ("mv1KJETikoyVdWeBh5Hr1SHBDycQUkrKFNdZ" : address)
let receiver : unit contract =
  match (Mavryk.get_contract_opt ownerAddress : unit contract option) with
    Some (contract) -> contract
  | None -> (failwith "Not a contract" : unit contract)
let payoutOperation : operation = Mavryk.transaction () (Mavryk.get_amount ()) receiver
let operations : operation list = [payoutOperation]