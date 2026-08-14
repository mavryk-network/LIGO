const ownerAddress : address = ("mv1KJETikoyVdWeBh5Hr1SHBDycQUkrKFNdZ" : address)
const receiver : contract (unit) =
  case (Mavryk.get_contract_opt (ownerAddress) : option (contract (unit))) of [
    Some (c) -> c
  | None -> (failwith ("Not a contract") : contract (unit))
  ]
const payoutOperation : operation = Mavryk.transaction (unit, Mavryk.get_amount (), receiver)
const operations : list (operation) = list [ payoutOperation ]