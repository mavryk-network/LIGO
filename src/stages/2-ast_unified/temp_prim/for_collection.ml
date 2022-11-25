type ('expr, 'statement) t =
  | ForMap of ('expr, 'statement) for_map
  | ForSetOrList of ('expr, 'statement) for_set_or_list

and ('expr, 'statement) for_map =
  { binding : Ligo_prim.Value_var.t * Ligo_prim.Value_var.t
  ; collection : 'expr
  ; block : 'statement Simple_utils.List.Ne.t
  }

and ('expr, 'statement) for_set_or_list =
  { var : Ligo_prim.Value_var.t
  ; for_kind : [ `Set | `List ]
  ; collection : 'expr
  ; block : 'statement Simple_utils.List.Ne.t
  }
[@@deriving yojson, map]