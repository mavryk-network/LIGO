type ('expr, 'branch) t =
  { test : 'expr
  ; ifso : 'branch
  ; ifnot : 'branch option [@sexp.option]
  }
[@@deriving yojson, map, iter, sexp]
