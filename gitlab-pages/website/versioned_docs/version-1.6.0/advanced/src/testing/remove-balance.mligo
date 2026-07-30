(* This is remove-balance.mligo *)

type balances = (address, mav) map

let remove_balances_under (b:balances) (threshold:mav) : balances =
  Map.fold
    (fun ((acc, (k, v)) : balances * (address * mav)) ->
       if v < threshold then Map.remove k acc else acc)
    b b