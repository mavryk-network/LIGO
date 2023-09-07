type 'expr t = 'expr list [@@deriving eq, compare, yojson, hash, sexp, fold, iter, map]

let pp f ppf ts =
  let open Simple_utils.PP_helpers in
  Format.fprintf ppf "@[<hv 2>[ %a ]@]" (list_sep f (tag " *@ ")) ts
