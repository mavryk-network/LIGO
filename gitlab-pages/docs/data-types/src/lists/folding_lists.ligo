function add1 (const a_i : int * int) : int is a_i.0 + a_i.1
const sum1 : int = List.fold_left (add1, 0, list [1; 2; 3])
function add2 (const i_a : int * int) : int is i_a.0 + i_a.1
const sum2 : int = List.fold_right (add2, list [1; 2; 3], 0)