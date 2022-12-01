type ('expr, 'stmt) fun_block =
  | FunctionBody of 'stmt Simple_utils.List.Ne.t
  | ExpressionBody of 'expr

and ('expr, 'ty_expr, 'stmt) t =
  { parameters : 'expr
  ; lhs_type : 'ty_expr option
  ; body : ('expr, 'stmt) fun_block
  }
[@@deriving yojson, map, sexp]