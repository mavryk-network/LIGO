type 'e t =
  { language : string
  ; code : 'e
  }
[@@deriving eq, compare, yojson, hash, fold, map, iter, sexp]

let pp f ppf { language; code } = Format.fprintf ppf "[%%%s %a]" language f code
