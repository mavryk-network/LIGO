type 't fun_type_arg =
  { name : Ligo_prim.Value_var.t option
  ; type_expr : 't
  }

and 't fun_type_args = 't fun_type_arg list [@@deriving yojson, map, fold, iter, sexp]

type 't t = 't fun_type_args * 't [@@deriving yojson, map, fold, iter, sexp]
