type 'e t = 'e el list

and 'e el =
  { ellipsis : bool
  ; pattern : 'e
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
