type t = {
  key      : string;
  value    : string option;
} [@@deriving yojson, sexp]