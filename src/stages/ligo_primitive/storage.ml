type 't t = { contract : 't } [@@deriving equal, compare, sexp, yojson, hash, map, fold]

let iter f { contract } = f contract

let pp pp_contract ppf { contract } =
  Format.fprintf ppf "@[%a storage@]" pp_contract contract
