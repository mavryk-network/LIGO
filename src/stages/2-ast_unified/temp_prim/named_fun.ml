type 't fun_type_arg =
  { name : string
  ; type_expr : 't
  }

and 't fun_type_args = 't fun_type_arg list [@@deriving yojson, map, iter, sexp]

type 't t = 't fun_type_args * 't [@@deriving yojson, map, iter, sexp]