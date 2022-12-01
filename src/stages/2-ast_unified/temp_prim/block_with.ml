type ('expr, 'stmt) t =
  { block : 'stmt Simple_utils.List.Ne.t
  ; expr : 'expr
  }
[@@deriving yojson, map, sexp]
