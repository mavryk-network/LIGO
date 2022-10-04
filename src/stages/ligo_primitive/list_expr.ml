type 'a t = 'a list
  [@@deriving eq, compare, yojson, hash, sexp, fold, map]

let fold_map f init t = List.fold_map ~f ~init t


let pp f ppf = fun m ->
  Format.fprintf ppf "list[%a]"
    Simple_utils.PP_helpers.(list_sep_d f) m
