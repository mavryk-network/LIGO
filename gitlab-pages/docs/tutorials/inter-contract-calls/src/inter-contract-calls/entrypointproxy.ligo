(* contracts/examples/ligo/EntrypointProxy.ligo *)

type parameter is int

type storage is address

function get_add_entrypoint (const addr : address) is
  case Mavryk.get_entrypoint_opt ("%add", addr) of [
    Some (contract) -> contract
  | None -> failwith ("The entrypoint does not exist")
  ]

[@entry]
function main (const param : parameter; const callee_addr : storage) : list (operation) * storage is
  block {
    const add : contract (int) = get_add_entrypoint (callee_addr);
    const op = Mavryk.transaction (param, 0mumav, add)
  } with (list [op], callee_addr)