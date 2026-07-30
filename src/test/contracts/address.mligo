let check (p : key_hash) =
  let c : unit contract = Mavryk.implicit_account p
  in Mavryk.address c
