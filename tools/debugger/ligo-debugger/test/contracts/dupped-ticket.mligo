[@entry]
let main (p : nat ticket) (s : int) : operation list * int =
  let kek = Mavryk.get_now () in
  let ((_, (value, _)), _) = Mavryk.read_ticket p in
  let ((_, (value2, _)), _) = Mavryk.read_ticket p in
  let s2 = if value + value2 > 10n then s * 2 else s / 2 in
  (([] : operation list), s2)
