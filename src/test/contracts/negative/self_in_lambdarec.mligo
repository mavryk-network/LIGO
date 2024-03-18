let rec foo (n : int) : address =
  if (n <= 1)
  then Mavryk.address (Mavryk.self "%default" : int contract)
  else
    let addr = foo (n - 1) in
    Mavryk.address
      (Option.unopt (Mavryk.get_contract_opt addr : int contract option))

[@entry]
let main (p : int) (_ : address) : (operation list * address) =
  let _dummy = foo p in
  (* force not to inline foo *)
  (([] : operation list), foo p)
