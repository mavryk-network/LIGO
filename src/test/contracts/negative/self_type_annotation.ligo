type parameter is nat
type storage is int
type return is list (operation) * storage


function main (const (p, s) : parameter * storage) : return is
  block {
    const self_contract: contract(int) = Tezos.self ("%default");
  }
  with ((nil: list(operation)), s)