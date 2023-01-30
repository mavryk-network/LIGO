(* Attributes *)

type key = string [@@deriving yojson]

type value =
  String of string
| Ident  of string [@@deriving yojson]

type attribute = key * value option [@@deriving yojson]

type t = attribute [@@deriving yojson]

val to_lexeme : attribute -> string
val to_string : attribute -> string
