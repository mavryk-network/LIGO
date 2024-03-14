type parameter = unit
type storage = unit
type result = operation list * storage

[@entry]
let no_tokens (action : parameter) (store : storage) : result =
  if Tezos.get_amount () > 0mav then
    failwith "This contract does not accept tokens."
  else ([], store)
let owner = ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe": address)

[@entry]
let owner_only (action : parameter) (store: storage) : result =
  if Tezos.get_sender () <> owner then failwith "Access denied."
  else ([], store)