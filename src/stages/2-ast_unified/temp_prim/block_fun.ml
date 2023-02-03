type ('expr, 'block) fun_block =
  | FunctionBody of 'block
  | ExpressionBody of 'expr

and ('expr, 'pattern, 'ty_expr, 'block) t =
  { parameters : 'pattern list
  ; lhs_type : 'ty_expr option
  ; body : ('expr, 'block) fun_block
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]