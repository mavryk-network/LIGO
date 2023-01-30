(* Attributes *)

type key = string [@@deriving yojson]

type value =
  String of string
| Ident  of string [@@deriving yojson]

type attribute = key * value option [@@deriving yojson]

type t = attribute [@@deriving yojson]

let sprintf = Printf.sprintf

let to_lexeme (key, value_opt) =
  match value_opt with
    None -> sprintf "[@%s]" key
  | Some String value -> sprintf "[@%s %s]" key value
  | Some Ident value ->  sprintf "[@%s %s]" key value

let to_string (key, value_opt) =
  match value_opt with
    None -> sprintf "%S" key
  | Some String value -> sprintf "(%S, Some (String %s))" key value
  | Some Ident value -> sprintf "(%S, Some (Ident %S))" key value
