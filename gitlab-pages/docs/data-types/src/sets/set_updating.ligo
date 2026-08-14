const nats : set (int) = Set.literal (list [3; 2; 2; 1])
const set_with_5 = Set.update (5, True, nats)
const set_without_3 = Set.update (3, False, nats)
function f (const x : int) : option (int) is
  if x mod 2 = 0n then None else Some (x)
// odds = Set.literal (list [3; 1])
const odds = Set.filter_map (f, nats)