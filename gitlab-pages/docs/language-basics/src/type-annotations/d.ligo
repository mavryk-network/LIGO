type parameter is
  Back of unit
| Claim of unit
| Withdraw of unit

type storage is record [
  owner    : address;
  goal     : mav;
  deadline : timestamp;
  backers  : map (address, mav);
  funded   : bool
]

[@entry]
function back (const _param : unit; const store : storage) : list (operation) * storage is
  if Mavryk.get_now () > store.deadline then failwith ("Deadline passed.")
  else
    case Map.find_opt (Mavryk.get_sender (), store.backers) of [
      None -> block {
        const backers = Map.update (Mavryk.get_sender (), Some (Mavryk.get_amount ()), store.backers)
      } with ((nil : list (operation)), store with record [backers = backers])
    | Some (_x) -> ((nil : list (operation)), store)
    ]