type 'a t = { module_ : 'a } [@@deriving eq, compare, yojson, hash]

let pp f ppf { module_ } = Format.fprintf ppf "open %a" f module_
