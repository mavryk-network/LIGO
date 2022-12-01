type 'expr t =
  { x : 'expr
  ; f : 'expr
  }
[@@deriving yojson, map, sexp]
