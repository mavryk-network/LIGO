type parameter is int
type storage is nat
type return is list (operation) * storage


function main (const p : parameter; const s : storage) : return is
  {
    const self_contract: contract(int) = Mavryk.self("%default") ;
    const op : operation = Mavryk.transaction (2, 300tz, self_contract) ;
  }
  with (list [op], s)