let check (kh : key_hash) : operation list =
  [Mavryk.set_delegate (Some kh)]