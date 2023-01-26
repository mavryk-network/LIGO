type assignment_operator =
  | Times_eq
  | Div_eq
  | Min_eq
  | Plus_eq
  | Mod_eq

and operator =
  | Eq
  | Assignment_operator of assignment_operator

and 'expr t =
  { expr1 : 'expr
  ; op : operator
  ; expr2 : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp]
