type st is sapling_state (8)
type tr is sapling_transaction (8)
const x = Mavryk.sapling_empty_state
function f (const tr : tr) : int * st is
  case Mavryk.sapling_verify_update (tr, x) of [
    Some (p) -> p.1
  | None -> (failwith ("failed") : int * st)
  ]