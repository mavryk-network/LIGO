type move = int * int
type register = (address, move) big_map

let empty : register = Big_map.empty
let moves : register =
  Big_map.literal [
    (("mv1XJ6kbMgDvXvvtw8KBG2Ne2ngNHxLfuUvE" : address), (1,2));
    (("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address), (0,3))]
let my_balance : move option =
  Big_map.find_opt ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address) moves
let has_balance : bool =
  Big_map.mem ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address) moves
let updated_map : register =
  Big_map.update
    ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address) (Some (4,9)) moves
let add (m : register) : register =
  Big_map.add
    ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k" : address) (4,9) m
let updated_map : register =
  Big_map.remove ("mv1Bbr38otexaqYQBJHHqV4uCYncf2y1HR9k": address) moves