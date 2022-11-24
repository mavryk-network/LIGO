type ('expr,'branch) t = {
  test         : 'expr;
  ifso         : 'branch;
  ifnot        : 'branch option;
} [@@deriving yojson, map, sexp]