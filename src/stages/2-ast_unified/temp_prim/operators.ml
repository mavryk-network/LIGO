[@@@warning "-30"]
type operator = string Simple_utils.Location.wrap

and 'expr binary_op =
  { operator : operator
  ; left : 'expr
  ; right : 'expr
  }

and 'expr unary_op =
  { operator : operator
  ; arg : 'expr
  }
[@@deriving yojson, map, sexp]
