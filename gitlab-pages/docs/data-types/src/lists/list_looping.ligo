function sum_list (const l : list (int)) : int is
  block {
    var sum : int := 0;
    for i in list l {
      sum := sum + i
    }
  } with sum