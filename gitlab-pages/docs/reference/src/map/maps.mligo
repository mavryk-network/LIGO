type move = int * int
type register = (address, move) map

let empty : register = Map.empty
let moves : register =
  Map.literal [
    (("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address), (1,2));
    (("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address), (0,3))]
let my_balance : move option =
  Map.find_opt ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address) moves
let updated_map : register =
  Map.update
    ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address) (Some (4,9)) moves
let (old_move_opt, updated_map) : move option * register =
  Map.get_and_update ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address) (Some (4, 9)) moves
let add (m : register) : register =
  Map.add
    ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address) (4,9) m
let updated_map : register =
  Map.remove ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k": address) moves
let iter_op (m : register) : unit =
  let predicate = fun (i,j : address * move) -> assert (j.0 > 3)
  in Map.iter predicate m
let map_op (m : register) : register =
  let increment = fun (_i,j : address * move) -> j.0, j.1 + 1
  in Map.map increment m
let fold_op (m : register) : int =
  let folded = fun (i,j : int * (address * move)) -> i + j.1.1
  in Map.fold folded m 5
let _ : nat = Map.size moves
let found : bool = Map.mem ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address)  moves