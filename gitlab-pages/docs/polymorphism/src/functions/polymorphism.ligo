function id<a> (const x : a) : a is x
function poly_map<a, b> (const f : a -> b; const l : list (a)) : list (b) is List.map (f, l)
const three_int : int = id (3);
const three_string : string = id ("three");