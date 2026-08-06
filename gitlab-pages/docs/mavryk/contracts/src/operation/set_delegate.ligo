function check (const kh : key_hash) : list (operation) is
  list [Mavryk.set_delegate (Some (kh))]