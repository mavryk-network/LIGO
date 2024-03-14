let destinationAddress : address =
  ("mv1XJ6kbMgDvXvvtw8KBG2Ne2ngNHxLfuUvE" : address)

let contract : unit contract =
  match (Tezos.get_contract_opt (Tezos.get_sender ()) : unit contract option) with
    Some contract -> contract
    | None -> (failwith "no contract" : unit contract)

let payment : operation =
  Tezos.transaction unit 100mumav contract
