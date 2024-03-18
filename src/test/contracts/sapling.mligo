type ss = 8 sapling_state

type storage = int * ss

type parameter = 8 sapling_transaction

type return = operation list * storage

[@entry]
let main (tr : parameter) (store : storage) : return =
  ([] : operation list),
  (let es : ss = Mavryk.sapling_empty_state in
   match Mavryk.sapling_verify_update tr es with
     Some (_, x) -> x
   | None -> (failwith "failed" : storage))
