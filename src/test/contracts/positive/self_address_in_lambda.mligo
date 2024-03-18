[@entry]
let main (_ : unit) (_ : unit -> address) : operation list * (unit -> address) =
  (([] : operation list), (fun (_ : unit) -> Mavryk.get_self_address ()))
