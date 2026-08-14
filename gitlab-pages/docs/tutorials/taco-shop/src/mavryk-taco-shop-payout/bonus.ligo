const ownerAddress : address = ("mv1KJETikoyVdWeBh5Hr1SHBDycQUkrKFNdZ" : address)
const donationAddress : address = ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)

const receiver : contract (unit) =
  case (Mavryk.get_contract_opt (ownerAddress) : option (contract (unit))) of [
    Some (c) -> c
  | None -> (failwith ("Not a contract") : contract (unit))
  ]

const donationReceiver : contract (unit) =
  case (Mavryk.get_contract_opt (donationAddress) : option (contract (unit))) of [
    Some (c) -> c
  | None -> (failwith ("Not a contract") : contract (unit))
  ]

const donationAmount : mav = (Mavryk.get_amount ()) / 10n

const operations : list (operation) =
  block {
    // Pedro will get 90% of the amount
    const op = case (Mavryk.get_amount ()) - donationAmount of [
      Some (x) -> Mavryk.transaction (unit, x, receiver)
    | None -> (failwith ("Insufficient balance") : operation)
    ]
  } with list [ op; Mavryk.transaction (unit, donationAmount, donationReceiver) ]