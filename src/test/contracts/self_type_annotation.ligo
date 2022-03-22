type parameter is nat
type storage is address
type return is list (operation) * storage


function main (const (_p, _s) : parameter * storage) : return is
  block {
    const s : contract(parameter) = Tezos.self("%default") ;
  }
  with ((nil: list(operation)), Tezos.address (s))