type ('lhs, 'rhs) t =
  | Punned of 'lhs
  | Complete of ('lhs * 'rhs)
[@@deriving yojson]