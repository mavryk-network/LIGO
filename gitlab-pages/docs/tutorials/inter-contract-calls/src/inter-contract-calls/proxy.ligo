(* examples/contracts/ligo/Proxy.ligo *)

type parameter is int

type storage is address

function get_contract (const addr : address) is
  case Mavryk.get_contract_opt (addr) of [
    Some (contract) -> contract
  | None -> failwith ("Callee does not exist")
  ]

[@entry]
function main (const param : parameter; const callee_addr : storage) : list (operation) * storage is
  block {
    const callee = get_contract (callee_addr);
    const op = Mavryk.transaction (param, 0mumav, callee)
  } with (list [op], callee_addr)