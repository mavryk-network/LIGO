type ('expr, 'ty_expr) t =
  { type_binder : string
  ; rhs : 'ty_expr
  ; body : 'expr
  }
[@@deriving yojson, map, sexp]
