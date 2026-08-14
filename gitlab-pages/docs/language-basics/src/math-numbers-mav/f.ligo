const a : int = 37
const b : int = 5
const ediv1 : option (int * nat) = ediv (a, b)  // Some (7, 2)
const c : nat = 37n
const ediv2 : option (int * nat) = ediv (c, b)  // Some (7, 2)
const d : nat = 5n
const ediv3 : option (nat * nat) = ediv (c, d)  // Some (7, 2)
const ediv4 : option (int * nat) = ediv (a, d)  // Some (7, 2)