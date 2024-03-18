let check (p : key_hash) : operation list =
  let _useless : operation = Mavryk.set_delegate (Some p)
  in ([] : operation list)
