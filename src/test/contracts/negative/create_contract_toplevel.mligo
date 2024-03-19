type return = operation list * string

[@entry]
let main (_ : string) (store : string) : return =
  let toto : operation * address = Mavryk.create_contract
    (fun (_p : nat) (_s : string) -> (([] : operation list), store))
    (None: key_hash option) 
    300mv 
    "un"
  in
  ([toto.0], store)
