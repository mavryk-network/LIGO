type parameter = Back | Claim | Withdraw

type storage = {
  owner    : address;
  goal     : mav;
  deadline : timestamp;
  backers  : (address, mav) map;
  funded   : bool
}

[@entry]
let back (param : unit) (store : storage) : operation list * storage = (* Annotation *)
  if Mavryk.get_now () > store.deadline then failwith "Deadline passed."
  else
    match Map.find_opt (Mavryk.get_sender ()) store.backers with
      None ->
        let backers = Map.update (Mavryk.get_sender ()) (Some (Mavryk.get_amount ())) store.backers
        in [], {store with backers=backers}
    | Some (x) -> [], store