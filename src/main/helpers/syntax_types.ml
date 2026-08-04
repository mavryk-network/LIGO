type s_syntax = Syntax_name of string

type t =
  | CameLIGO
  | JsLIGO
  | PascaLIGO (* MAVRYK: PascaLIGO *)
[@@deriving eq, ord]
