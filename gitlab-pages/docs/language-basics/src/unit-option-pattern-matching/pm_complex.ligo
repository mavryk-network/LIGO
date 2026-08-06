type complex_t is record [ a : option (list (int)); b : list (int) ]

function complex (const x : complex_t; const y : complex_t) : int is
  case (x, y) of [
    (record [a=None; b=_b1], record [a=_a2; b=_b2]) -> -1
  | (record [a=_a3; b=_b3], record [a=Some (nil); b=(hd # tl)]) -> hd
  | (record [a=_a4; b=_b4], record [a=Some (hd # tl); b=nil]) -> hd
  | (record [a=Some (a); b=_b5], _) -> int (List.length (a))
  ]