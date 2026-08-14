const s : set (int) = Set.literal (list [5; 1; 2; 2])
// plus_one = Set.literal (list [6; 2; 3])
function incr (const i : int) : int is i + 1
const plus_one : set (int) = Set.map (incr, s)