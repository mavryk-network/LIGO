type ('pattern, 'ty_expr) t =
  { param_kind : [ `Var | `Const ]
  ; pattern : 'pattern
  ; param_type : 'ty_expr option
  }
[@@deriving yojson, map, sexp]
