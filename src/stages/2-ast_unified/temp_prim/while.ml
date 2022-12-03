type ('expr, 'statement) t =
  { cond : 'expr
  ; block : 'statement Simple_utils.List.Ne.t
  }
[@@deriving yojson, map, iter, sexp]
