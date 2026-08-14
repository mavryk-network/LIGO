const my_map : map (int, string) =
  Map.literal (list [(1,"one"); (2,"two")])
const contains_2 : bool = Map.mem (2, my_map) // = true
const v : option (string) = Map.find_opt (2, my_map)
function force_access (const key : int; const m : map (int, string)) : string is
  case Map.find_opt (key, m) of [
    Some (value) -> value
  | None -> failwith ("No value.")
  ]