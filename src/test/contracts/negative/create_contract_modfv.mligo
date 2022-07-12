type return = operation list * string

let main (action, store : string * string) : return =
  module Foo = struct
    let store = store
  end in
  let toto : operation * address = Tezos.create_contract
    (fun (p, s : nat * string) -> (([] : operation list), Foo.store)) 
    (None: key_hash option) 
    300tz 
    "un"
  in
  ([toto.0], store)
