let destinationAddress : address =
  ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)

let contract : unit contract =
  match (Mavryk.get_contract_opt (Mavryk.get_sender ()) : unit contract option) with
    Some contract -> contract
    | None -> (failwith "no contract" : unit contract)

let payment : operation =
  Mavryk.transaction unit 100mumav contract
