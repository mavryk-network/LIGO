type 't t =
  { param_type : 't
  ; return_type : 't
  }
[@@deriving equal, compare, sexp, yojson, hash, map, fold]

let fold_map f acc { param_type; return_type } =
  let acc, param_type = f acc param_type in
  let acc, return_type = f acc return_type in
  acc, { param_type; return_type }


let iter f { param_type; return_type } =
  f param_type;
  f return_type


let pp pp_t ppf { param_type; return_type } =
  Format.fprintf ppf "@[(%a, %a) view@]" pp_t param_type pp_t return_type
