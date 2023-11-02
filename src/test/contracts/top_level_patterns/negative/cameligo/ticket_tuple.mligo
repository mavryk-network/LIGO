let (b, _) = (Option.unopt (Mavryk.create_ticket "one" 10n), 1)

type storage = string ticket

let main (_,_ : unit * storage) : operation list * storage
  = [], Option.unopt (Mavryk.join_tickets (b, b))