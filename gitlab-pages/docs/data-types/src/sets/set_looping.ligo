function sum_elt (const s : set (int)) : int is
  block {
    var sum : int := 0;
    for e in set s {
      sum := sum + e
    }
  } with sum