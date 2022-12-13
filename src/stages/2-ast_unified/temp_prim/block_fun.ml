type ('expr, 'stmt) fun_block =
  | FunctionBody of 'stmt Simple_utils.List.Ne.t
  | ExpressionBody of 'expr

and ('expr, 'pattern, 'ty_expr, 'stmt) t =
  { parameters : 'pattern Simple_utils.List.Ne.t
  ; lhs_type : 'ty_expr option
  ; body : ('expr, 'stmt) fun_block
  }
[@@deriving yojson, map, iter, sexp]