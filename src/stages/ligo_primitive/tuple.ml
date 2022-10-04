type 'a t = 'a list [@@deriving eq, compare, hash, yojson, map, fold]

let fold_map f init t = List.fold_map ~f ~init t

let pp_expr ppa ppf t =
  Format.fprintf
    ppf
    "@[<hv 2>( %a )@]"
    Simple_utils.PP_helpers.(list_sep ppa (tag " , "))
    t

let pp_type ppa ppf t = 
  Format.fprintf
    ppf
    "@[<hv 2>( %a )@]"
    Simple_utils.PP_helpers.(list_sep ppa (tag " *@ "))
    t
