let (b, _) = (Option.unopt (Mavryk.create_ticket "one" 10n), 1)

type storage = string ticket

[@entry]
let main (_ : unit) (_ : storage) : operation list * storage =
  [], Option.unopt (Mavryk.join_tickets (b, b))
