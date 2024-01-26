type parameter = int

type storage = address

let get_add_entrypoint (addr : address) =
  match (Mavryk.get_entrypoint_opt "%add" addr : int contract option) with
    Some contract -> contract
  | None -> (failwith "The entrypoint does not exist" : int contract)

let main (param, callee_addr : parameter * storage) =
  let add : int contract = get_add_entrypoint (callee_addr) in
  let op = Mavryk.transaction param 0mumav add in
  [op], callee_addr
