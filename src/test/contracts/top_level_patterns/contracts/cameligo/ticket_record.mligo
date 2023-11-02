type t = { a : int ticket ; b : string ticket ; c : nat ticket }

let { a = a1 ; b = b1 ; c = c1 }
    = { a = Option.unopt (Mavryk.create_ticket 1 10n)
      ; b = Option.unopt (Mavryk.create_ticket "one" 10n)
      ; c = Option.unopt (Mavryk.create_ticket 1n 10n)
      }

let { a = a2 ; c = c2 ; b = b2 }
    = { a = Option.unopt (Mavryk.create_ticket 2 10n)
      ; b = Option.unopt (Mavryk.create_ticket "TWO" 10n)
      ; c = Option.unopt (Mavryk.create_ticket 3n 10n)
      }

type storage = int ticket * string ticket * nat ticket

let main (_,_ : unit * storage) : operation list * storage
  = [],
    (let a = Option.unopt (Mavryk.join_tickets (a1, a2)) in
    let b  = Option.unopt (Mavryk.join_tickets (b1, b2)) in
    let c  = Option.unopt (Mavryk.join_tickets (c1, c2)) in
    (a, b, c))