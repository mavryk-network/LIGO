type 'expr t =
  { expr : 'expr
  ; selection : 'expr Selection.t
  }
[@@deriving yojson, map, sexp]