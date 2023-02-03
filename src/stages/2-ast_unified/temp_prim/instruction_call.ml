type 'expr t = 'expr * 'expr list [@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
