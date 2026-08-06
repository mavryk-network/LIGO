const my_set : set (int) = Set.empty
const my_set : set (int) = Set.literal (list [3; 2; 2; 1])
const with_999 : set (int) = Set.add (999, my_set)
const contains_3 : bool = Set.mem (3, my_set)
const cardinal : nat = Set.size (my_set)
const larger_set : set (int) = Set.add (4, my_set)
const smaller_set : set (int) = Set.remove (3, my_set)
function assert_all_greater_than_three (const s : set (int)) : unit is
  Set.iter (function (const i : int) : unit is assert (i > 3), s)
function sum (const acc_i : int * int) : int is acc_i.0 + acc_i.1
const sum_of_elements : int = Set.fold (sum, my_set, 0)