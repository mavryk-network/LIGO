function assert_all_greater_than_3 (const m : map (int, int)) : unit is
  block {
    function check (const kv : int * int) : unit is assert (kv.1 > 3)
  } with Map.iter (check, m) // The key is discarded