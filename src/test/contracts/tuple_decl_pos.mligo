
let c () : operation * address =
  Tezos.create_contract (fun ((_, _) : unit * unit) -> ([] : operation list), ()) (None : key_hash option) 0tez ()

let foo =
  let (_a, _b) : operation * address = c () in
  ()
