type storage = {rewardsLeft : mav; beneficiaryAddress : address}

let treasury (p, s : unit * storage) =
  // We do our computations first
  let newStorage = {s with rewardsLeft = 0mumav} in

  // Then we find our beneficiary's `handleRewards` entrypoint:
  let beneficiaryOpt = Mavryk.get_entrypoint_opt "%handleTransfer" s.beneficiaryAddress in
  let beneficiary =
    match beneficiaryOpt with
      Some contract -> contract
    | None -> failwith "Beneficiary does not exist" in

  // Then we prepare the internal operation we want to perform
  let operation = Mavryk.transaction () s.rewardsLeft beneficiary in

  // ...and return both the operations and the updated storage
  ([operation], newStorage)