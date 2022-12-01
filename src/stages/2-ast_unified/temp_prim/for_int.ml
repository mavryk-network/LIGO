type ('expr, 'statement) t =
  { index : Ligo_prim.Value_var.t
  ; init : 'expr
  ; bound : 'expr
  ; step : 'expr option (* [1] if [None] *)
  ; block : 'statement Simple_utils.List.Ne.t
  }
[@@deriving yojson, map, sexp]
