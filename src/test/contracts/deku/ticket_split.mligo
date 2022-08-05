type parameter = int
type storage = int

let main ((_,_):(parameter * storage)) =
    let a = Tezos.create_ticket 1 10n in
    match Tezos.split_ticket a (5n, 5n) with 
      Some split_tickets -> ([]: operation list), 1
    | None -> ([]: operation list), 0


    