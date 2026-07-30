type return = operation list * string

[@entry]
let main (_ : string) (store : string) : return =
  [@thunk]
  let sender = Mavryk.get_sender () in
  let toto : operation * address =
    Mavryk.create_contract
      (fun (_ : nat) (_ : address) -> (([] : operation list), sender))
      (None : key_hash option)
      1000000mumav
      sender in
  ([toto.0], store)
