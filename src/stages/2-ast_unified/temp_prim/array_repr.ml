type 'expr item =
  | Expr_entry of 'expr
  | Rest_entry of 'expr

and 'expr t = 'expr item list [@@deriving yojson, map, sexp]
