const my_map : big_map (int, string) =
  Big_map.literal (list [(1,"one"); (2,"two")])
const contains_2 : bool = Big_map.mem (2, my_map) // = True
const v : option (string) = Big_map.find_opt (2, my_map)
function force_access (const key : int; const m : big_map (int, string)) : string is
  case Big_map.find_opt (key, m) of [
    Some (value) -> value
  | None -> failwith ("No value.")
  ]