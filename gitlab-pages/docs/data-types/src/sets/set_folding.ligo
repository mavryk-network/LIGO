const s : set (int) = Set.literal (list [1; 2; 3])

function fold_incr (const a_i : list (int) * int) : list (int) is
  a_i.1 # a_i.0
// incr = list [3; 2; 1]
const incr : list (int) = Set.fold (fold_incr, s, (list [] : list (int)))

function fold_decr (const i_a : int * list (int)) : list (int) is
  i_a.0 # i_a.1
// decr = list [1; 2; 3]
const decr : list (int) = Set.fold_desc (fold_decr, s, (list [] : list (int)))