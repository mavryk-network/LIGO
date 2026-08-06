function sum_val (const m : map (int, int)) : int is
  block {
    var sum : int := 0;
    for _key -> val in map m {
      sum := sum + val  // The key is discarded.
    }
  } with sum