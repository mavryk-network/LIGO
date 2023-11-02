type return = operation list * string

let main (_action, store : string * string) : return =
  [@thunk] let sender = Mavryk.get_sender () in
  let toto : operation * address = Mavryk.create_contract
    (fun (_, _ : nat * address) -> (([] : operation list), sender)) 
    (None: key_hash option) 
    1tz
    sender
  in
  ([toto.0], store)
  
