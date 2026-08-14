const my_map : map (int, int) = Map.literal (list [(0,0); (1,1); (2,2)])
// plus_one = Map.literal (list [(0,0); (1,2); (2,4)])
function combine (const kv : int * int) : int is kv.0 + kv.1
const plus_one = Map.map (combine, my_map)