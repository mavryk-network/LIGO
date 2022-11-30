type ('expr,'ty_expr,'pattern) t =
  { type_params : Ligo_prim.Type_var.t Simple_utils.List.Ne.t option
  ; parameters : ('pattern,'ty_expr) Param.t list
  ; ret_type : 'ty_expr option
  ; body : 'expr
  }
[@@deriving yojson, map, sexp]