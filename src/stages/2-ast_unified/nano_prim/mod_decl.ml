type ('mod_expr, 'signature) t =
  { name : Ligo_prim.Module_var.t
  ; mod_expr : 'mod_expr
  ; annotation : 'signature option
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
