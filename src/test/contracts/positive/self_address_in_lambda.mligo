let main (_, _ : unit * (unit -> address)) : operation list * (unit -> address) =
  (([] : operation list), (fun (_ : unit) -> Mavryk.get_self_address ()))
