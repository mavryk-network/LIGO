type ('lhs, 'ty) t =
  { module_path : 'lhs
  ; field : 'ty
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
