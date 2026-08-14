function assert_all_greater_than_3 (const s : set (int)) : unit is
  block {
    function check (const i : int) : unit is assert (i > 3)
  } with Set.iter (check, s)