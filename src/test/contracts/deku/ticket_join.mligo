type parameter = int
type storage = int

let main ((_,_):(parameter * storage)) =
    let a = Tezos.create_ticket 1 10n in
    let b = Tezos.create_ticket 1 10n in
    let _joined = Tezos.join_tickets (a, b) in
    ([]: operation list), 0


    