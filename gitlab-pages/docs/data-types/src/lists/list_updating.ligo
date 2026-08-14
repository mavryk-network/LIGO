const nats : list (int) = list [0; 1; 2; 3; 4]
// evens_zeroed = [0; 1; 0; 3; 0]
function is_even (const x : int) : bool is x mod 2 = 0n
const evens_zeroed = List.update_with (is_even, 0, nats)
function f (const x : int) : option (int) is
  if x mod 2 = 0n then None else Some (x * x)
// odds_squared = [0; 1; 2; 9; 4]
const odds_squared = List.update (f, nats)