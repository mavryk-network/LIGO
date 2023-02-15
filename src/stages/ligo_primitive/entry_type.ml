type 't t = { param_type : 't } [@@deriving equal, compare, sexp, yojson, hash, map, fold]

let iter f { param_type } = f param_type

let fold_map f acc { param_type } =
  let acc, param_type = f acc param_type in
  acc, { param_type }


let pp pp_t ppf { param_type } = Format.fprintf ppf "@[%a entry@]" pp_t param_type
