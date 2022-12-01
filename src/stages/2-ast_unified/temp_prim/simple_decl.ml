type ('binder, 'expr, 'ty_expr) t =
  { type_params : Ligo_prim.Type_var.t Simple_utils.List.Ne.t option
  ; pattern : 'binder
  ; rhs_type : 'ty_expr option
  ; let_rhs : 'expr
  }
[@@deriving yojson, map, sexp]
