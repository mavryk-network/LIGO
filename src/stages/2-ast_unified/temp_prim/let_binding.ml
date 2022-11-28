type ('pattern, 'expr, 'ty_expr) t =
  { is_rec : bool
  ; type_params : Ligo_prim.Type_var.t Simple_utils.List.Ne.t option
  ; lhs : 'pattern Simple_utils.List.Ne.t
  ; rhs_type : 'ty_expr option
  ; rhs : 'expr
  ; body : 'expr
  }
[@@deriving yojson, map]
