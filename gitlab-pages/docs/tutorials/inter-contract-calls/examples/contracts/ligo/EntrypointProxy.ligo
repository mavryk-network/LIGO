type parameter is int

type storage is address

function get_add_entrypoint (const addr : address) is {
  const entrypoint : option (contract (int))
  = Mavryk.get_entrypoint_opt ("%add", addr)
} with
    case entrypoint of [
      Some (contract) -> contract
    | None -> (failwith ("The entrypoint does not exist") : contract (int))
    ]

function main (const param : parameter; const callee_addr : storage) is {
  const add : contract (int) = get_add_entrypoint (callee_addr);
  const op = Mavryk.transaction (param, 0mumav, add)
} with (list [op], callee_addr)
