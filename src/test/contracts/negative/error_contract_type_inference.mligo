type parameter = int

type storage = address

let get_add_entrypoint (addr : address) =
  match (Mavryk.get_entrypoint_opt "%add" addr) with
    Some contract -> contract
  | None -> (failwith "The entrypoint does not exist" : int contract)

let main (param : parameter) (callee_addr : storage) =
  let add : int contract = get_add_entrypoint (callee_addr) in
  let op = Mavryk.transaction param 0mumav add in
  [op], callee_addr
