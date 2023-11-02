type parameter is nat
type storage is address
type return is list (operation) * storage


function main (const _p : parameter; const _s : storage) : return is
  {
    const s : contract(parameter) = Mavryk.self("%default") ;
  }
  with ((nil: list(operation)), Mavryk.address (s))