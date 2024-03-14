let ownerAddress : address = ("mv1KJETikoyVdWeBh5Hr1SHBDycQUkrKFNdZ" : address)
let donationAddress : address = ("mv1XJ6kbMgDvXvvtw8KBG2Ne2ngNHxLfuUvE" : address)

let receiver : unit contract =
  match ((Tezos.get_contract_opt ownerAddress) : unit contract option) with
    Some contract -> contract
  | None -> ((failwith "Not a contract") : unit contract)

let donationReceiver : unit contract  =
  match ((Tezos.get_contract_opt donationAddress) : unit contract option) with
    Some contract -> contract
  | None -> ((failwith "Not a contract") : unit contract)

let donationAmount : tez = (Tezos.get_amount ()) / 10n

let operations : operation list =
    // Pedro will get 90% of the amount
    let op = match ((Tezos.get_amount ()) - donationAmount) with
      | Some x -> Tezos.transaction () x receiver
      | None -> (failwith "Insufficient balance")
    in
    [ op ; Tezos.transaction () donationAmount donationReceiver ]