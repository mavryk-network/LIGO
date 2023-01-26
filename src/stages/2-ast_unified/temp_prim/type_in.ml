type ('expr, 'ty_expr) t =
  { type_binder : Ligo_prim.Type_var.t
  ; rhs : 'ty_expr
  ; body : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp]
