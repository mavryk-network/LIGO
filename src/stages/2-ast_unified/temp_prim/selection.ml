type 'expr t =
  | FieldName of Ligo_prim.Label.t
  | Component_num of (string * Simple_utils.Z.t)
  | Component_expr of 'expr
[@@deriving yojson, map, iter, sexp]