type 'expr element =
  | Spread of 'expr
  | Expr of 'expr
[@@deriving eq, compare, yojson, hash, sexp, fold, iter, map]

type 'expr t = 'expr element list
[@@deriving eq, compare, yojson, hash, sexp, fold, iter, map]

let pp_element f ppf = function
  | Spread e -> Format.fprintf ppf "...%a" f e
  | Expr e -> f ppf e


let pp f ppf (ts : 'expr t) =
  let open Simple_utils.PP_helpers in
  Format.fprintf ppf "@[<hv 2>[ %a ]@]" (list_sep (pp_element f) (tag " *@ ")) ts
