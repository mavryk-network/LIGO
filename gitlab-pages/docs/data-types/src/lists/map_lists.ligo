function increment (const i : int) : int is i + 1
const plus_one : list (int) = List.map (increment, list [6; 2; 3; 3])