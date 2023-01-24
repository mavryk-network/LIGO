type ('expr, 'ty_expr, 'lambda) t =
  { fun_name : Ligo_prim.Value_var.t
  ; fun_type : 'ty_expr
  ; lambda : 'lambda
  }
[@@deriving eq, compare, yojson, sexp, iter, map]