type move = int * int
type register = (address, move) map
let empty : register = Map.empty
let moves : register =
  Map.literal [
    (("mv1XJ6kbMgDvXvvtw8KBG2Ne2ngNHxLfuUvE" : address), (1,2));
    (("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address), (0,3))]
let my_balance : move option =
  Map.find_opt ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address) moves
let force_access (key, moves : address * register) : move =
  match Map.find_opt key moves with
    Some move -> move
  | None -> failwith "No move."
let assign (m : register) : register =
  Map.update
    ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address) (Some (4,9)) m
let add (m : register) : register =
  Map.add
    ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address) (4,9) m
let delete (key, moves : address * register) : register =
  Map.remove key moves
let iter_op (m : register) : unit =
  let predicate = fun (i,j : address * move) -> assert (j.0 > 3)
  in Map.iter predicate m
let map_op (m : register) : register =
  let increment = fun (_,j : address * move) -> j.0, j.1 + 1
  in Map.map increment m
let fold_op (m : register) : int =
  let folded = fun (i,j : int * (address * move)) -> i + j.1.1
  in Map.fold folded m 5