const my_map : big_map (int, string) =
  Big_map.literal (list [(1,"one"); (2,"two")])
const map_with_3 = Big_map.update (3, Some ("three"), my_map)
const contains_3 : bool = Big_map.mem (3, map_with_3) // = True
const map_without_2 = Big_map.update (2, (None : option (string)), my_map)
const contains_2 : bool = Big_map.mem (2, map_without_2) // = False
// three = Some "three"
const (three, map_without_3) = Big_map.get_and_update (3, (None : option (string)), map_with_3)