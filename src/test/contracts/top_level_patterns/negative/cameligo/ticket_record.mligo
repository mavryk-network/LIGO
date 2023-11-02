type t = { b : string ticket }

let { b } = { b = Option.unopt (Mavryk.create_ticket "one" 10n) }

type storage = string ticket

let main (_,_ : unit * storage) : operation list * storage
  = [], Option.unopt (Mavryk.join_tickets (b, b))