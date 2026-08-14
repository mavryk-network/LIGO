function is_it_a_nat (const i : int) : bool is
  case is_nat (i) of [
    None -> False
  | Some (_n) -> True
  ]