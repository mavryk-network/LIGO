let my_ticket1 = Option.unopt (Mavryk.create_ticket 1 10n)
let my_ticket2 = Option.unopt (Mavryk.create_ticket "one" 10n)
let v =
  let (_addr, (payload, _amt)), _ticket = Mavryk.read_ticket my_ticket1
  in payload
let ta, tb =
  match Mavryk.split_ticket my_ticket1 (6n, 4n) with
    None -> failwith "amt_a + amt_v <> amt"
  | Some split_tickets -> split_tickets
let tc : int ticket option =
  let ta = Option.unopt (Mavryk.create_ticket 1 10n) in
  let tb = Option.unopt (Mavryk.create_ticket 1 5n) in
  Mavryk.join_tickets (ta, tb)