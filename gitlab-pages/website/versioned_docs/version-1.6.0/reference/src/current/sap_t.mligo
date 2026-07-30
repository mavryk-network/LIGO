type st = 8 sapling_state
type tr = 8 sapling_transaction
let x = Mavryk.sapling_empty_state
let f (tr : tr) =
  match Mavryk.sapling_verify_update tr x with
    Some (_, x) -> x
  | None -> (failwith "failed" : int * st)