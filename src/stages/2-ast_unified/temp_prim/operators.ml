[@@@warning "-30"]
type operator = string Simple_utils.Location.wrap
[@@deriving yojson, map, iter, sexp]

type 'expr binary_op =
  { operator : operator
  ; left : 'expr
  ; right : 'expr
  }
[@@deriving yojson, map, iter, sexp]

type 'expr unary_op =
  { operator : operator
  ; arg : 'expr
  }
[@@deriving yojson, map, iter, sexp]
