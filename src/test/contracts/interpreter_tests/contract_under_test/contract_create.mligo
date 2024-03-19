type storage = address option
type return = operation list * storage
type parameter = One | Two

[@entry]
let main (action : parameter) (store : storage) : return =
  match action with
    | One ->
      let a = match store with
        | None -> (failwith 1 : address)
        | Some a -> a
      in
      let c : nat contract option = Mavryk.get_contract_opt a in
      let ops = match c with
          Some (c) -> [ Mavryk.transaction 1n 10mav c ]
        | None     -> (failwith 2 : operation list)
      in
      (ops, (None: storage))
    | Two ->
      let x : operation * address = Mavryk.create_contract
        (fun (_p : nat) (_s : string) -> (failwith 111: operation list * string))
        (None: key_hash option) 
        1mv
        "un"
      in
      ([x.0], (Some x.1))
