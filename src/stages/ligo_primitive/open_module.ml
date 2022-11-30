type 'a t = { module_ : 'a } [@@deriving eq, compare, yojson, hash]

let pp f ppf { module_ } = Format.fprintf ppf "@[<2>open %a@.@]" f module_
