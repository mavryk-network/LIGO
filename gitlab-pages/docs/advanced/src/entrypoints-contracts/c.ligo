type parameter is unit
type storage is unit
type result is list (operation) * storage

[@entry]
function no_tokens (const action : parameter; const store : storage) : result is
  if Mavryk.get_amount () > 0mav then
    failwith ("This contract does not accept tokens.")
  else ((nil : list (operation)), store)
const owner : address = "mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe"

[@entry]
function owner_only (const action : parameter; const store : storage) : result is
  if Mavryk.get_sender () =/= owner then failwith ("Access denied.")
  else ((nil : list (operation)), store)