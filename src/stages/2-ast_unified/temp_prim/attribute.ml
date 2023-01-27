type t =
  { key : string
  ; value : string option
  }
[@@deriving yojson, iter, fold, sexp]

let make key v = { key ; value = Some v}