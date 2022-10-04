type 'a t = 'a list [@@deriving eq, compare, hash, yojson]

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
