const my_ticket1 = Option.unopt (Mavryk.create_ticket (1, 10n))
const my_ticket2 = Option.unopt (Mavryk.create_ticket ("one", 10n))
const v : int =
  block {
    const ((_addr, (payload, _amt)), _ticket) = Mavryk.read_ticket (my_ticket1);
  } with payload
const (ta, tb) =
  case Mavryk.split_ticket (my_ticket1, (6n, 4n)) of [
    None -> (failwith ("amt_a + amt_v =/= amt") : ticket (int) * ticket (int))
  | Some (split_tickets) -> split_tickets
  ]
const tc : option (ticket (int)) =
  block {
    const ta = Option.unopt (Mavryk.create_ticket (1, 10n));
    const tb = Option.unopt (Mavryk.create_ticket (1, 5n));
  } with Mavryk.join_tickets ((ta, tb))