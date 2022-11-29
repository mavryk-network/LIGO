type ('expr, 'statement) case =
  | Switch_case of 'expr * 'statement Simple_utils.List.Ne.t option
  | Switch_default_case of 'statement Simple_utils.List.Ne.t option

and ('expr, 'statement) t =
  { switch_expr : 'expr
  ; switch_cases : ('expr, 'statement) case Simple_utils.List.Ne.t
  }
[@@deriving yojson, map, sexp]
