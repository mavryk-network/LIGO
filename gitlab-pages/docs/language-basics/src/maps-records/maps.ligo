type move is int * int
type register is map (address, move)
const empty : register = Map.empty
const moves : register =
  Map.literal (list [
    (("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address), (1,2));
    (("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address), (0,3))])
const my_balance : option (move) =
  Map.find_opt (("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address), moves)
function force_access (const key : address; const moves : register) : move is
  case Map.find_opt (key, moves) of [
    Some (mv) -> mv
  | None -> failwith ("No move.")
  ]
function assign (const m : register) : register is
  Map.update (("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address), Some (4,9), m)
function add (const m : register) : register is
  Map.add (("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address), (4,9), m)
function delete (const key : address; const moves : register) : register is
  Map.remove (key, moves)
function iter_op (const m : register) : unit is
  block {
    function predicate (const kv : address * move) : unit is
      assert (kv.1.0 > 3)
  } with Map.iter (predicate, m)
function map_op (const m : register) : register is
  block {
    function increment (const kv : address * move) : move is (kv.1.0, kv.1.1 + 1)
  } with Map.map (increment, m)
function fold_op (const m : register) : int is
  block {
    function folded (const acc_kv : int * (address * move)) : int is acc_kv.0 + acc_kv.1.1.1
  } with Map.fold (folded, m, 5)