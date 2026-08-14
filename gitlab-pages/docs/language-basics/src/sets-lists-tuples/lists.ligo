const empty_list : list (int) = list []
const my_list : list (int) = list [1; 2; 2]  // The head is 1, the tail is [2; 2]
const larger_list : list (int) = 5 # my_list  // [5;1;2;2]
const head : option (int) = List.head_opt (my_list)
const tail : option (list (int)) = List.tail_opt (my_list)
function assert_all_greater_than_three (const l : list (int)) : unit is
  List.iter (function (const i : int) : unit is assert (i > 3), l)
function increment (const i : int) : int is i + 1

// Creates a new list with all elements incremented by 1
const plus_one : list (int) = List.map (increment, larger_list)  // [6,2,3,3]
function sum (const acc_i : int * int) : int is acc_i.0 + acc_i.1
const sum_of_elements : int = List.fold_left (sum, 0, my_list)