type t = {b : string ticket}

let {b} = {b = Option.unopt (Mavryk.create_ticket "one" 10n)}

type storage = string ticket

[@entry]
let main (_ : unit) (_ : storage) : operation list * storage =
  [], Option.unopt (Mavryk.join_tickets (b, b))
