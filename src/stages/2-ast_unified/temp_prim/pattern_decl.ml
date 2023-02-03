type ('expr, 'pattern) t =
{ pattern :'pattern
; expr : 'expr
}
[@@deriving eq, compare, yojson, iter, fold, map, sexp, eq, compare, hash]
