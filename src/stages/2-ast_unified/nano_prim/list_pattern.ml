type 'e t =
  | Cons of 'e * 'e
  | List of 'e list
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
