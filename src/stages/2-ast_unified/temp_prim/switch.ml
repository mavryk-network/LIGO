type ('expr, 'statement) case =
  | Switch_case of 'expr * 'statement Simple_utils.List.Ne.t option
  | Switch_default_case of 'statement Simple_utils.List.Ne.t option

and ('expr, 'statement) t =
  { switchee : 'expr
  ; cases : ('expr, 'statement) case Simple_utils.List.Ne.t
  }
[@@deriving yojson, map, iter, sexp]
