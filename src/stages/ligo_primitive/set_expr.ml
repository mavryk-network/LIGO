type 'a t = 'a list
  [@@deriving eq, compare, yojson, hash, sexp, map, fold]


let fold_map f init t = List.fold_map ~init ~f t

let pp f ppf = fun m ->
  Format.fprintf ppf "set[%a]"
    Simple_utils.PP_helpers.(list_sep_d f) m


