(* examples/contracts/mligo/Proxy.mligo *)

type parameter = int

type storage = address

let get_contract (addr : address) =
  match Mavryk.get_contract_opt addr with
    Some contract -> contract
  | None -> failwith "Callee does not exist"

[@entry]
let main (param : parameter) (callee_addr : storage) =
  let callee = get_contract (callee_addr) in
  let op = Mavryk.transaction param 0mumav callee in
  [op], callee_addr