type 'expr t =
  { map : 'expr
  ; keys : 'expr Simple_utils.List.Ne.t
  }
[@@deriving yojson, map, sexp]
