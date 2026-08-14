const destinationAddress : address =
  ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)

const dest_contract : contract (unit) =
  case (Mavryk.get_contract_opt (Mavryk.get_sender ()) : option (contract (unit))) of [
    Some (c) -> c
  | None -> (failwith ("no contract") : contract (unit))
  ]

const payment : operation =
  Mavryk.transaction (unit, 100mumav, dest_contract)
