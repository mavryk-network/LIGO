let ownerAddress : address = ("mv1KJETikoyVdWeBh5Hr1SHBDycQUkrKFNdZ" : address)
let donationAddress : address = ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)

let receiver : unit contract =
  match ((Mavryk.get_contract_opt ownerAddress) : unit contract option) with
    Some contract -> contract
  | None -> ((failwith "Not a contract") : unit contract)

let donationReceiver : unit contract  =
  match ((Mavryk.get_contract_opt donationAddress) : unit contract option) with
    Some contract -> contract
  | None -> ((failwith "Not a contract") : unit contract)

let donationAmount : mav = (Mavryk.get_amount ()) / 10n

let operations : operation list =
    // Pedro will get 90% of the amount
    let op = match ((Mavryk.get_amount ()) - donationAmount) with
      | Some x -> Mavryk.transaction () x receiver
      | None -> (failwith "Insufficient balance")
    in
    [ op ; Mavryk.transaction () donationAmount donationReceiver ]