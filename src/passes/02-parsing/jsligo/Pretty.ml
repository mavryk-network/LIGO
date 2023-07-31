(* A pretty printer for JsLIGO *)

[@@@warning "-42"]

(* Jane Street dependency *)

module List = Core.List

(* Vendored dependencies *)

module Utils  = Simple_utils.Utils
module Region = Simple_utils.Region
module Option = Simple_utils.Option

(* Local dependencies *)

module CST = Cst_jsligo.CST
module PrettyComb = Parsing_shared.PrettyComb

(* Global openings *)

open CST
open! Region
open! PPrint

(* Utilities and local shadowings *)

let prefix = PrettyComb.prefix
let (^/^)  = PrettyComb.(^/^)
type state = PrettyComb.state

(* Placement *)

let default_state : PrettyComb.state =
  object
    method indent       = 2
    method leading_vbar = PrettyComb.Only_on_new_line
  end

(* Comments *)

let pp_line_comment comment = string "//" ^^ string comment.value

let pp_block_comment comment =
  string "/*" ^^ string comment.value ^^ string "*/"

let pp_comment = function
  Wrap.Block comment -> pp_block_comment comment
| Wrap.Line  comment -> pp_line_comment  comment

let pp_comments = function
  [] -> empty
| comments -> separate_map hardline pp_comment comments ^^ hardline

(* Tokens *)

let pp_line_comment_opt prefix = function
  None -> prefix
| Some comment -> prefix ^^ space ^^ pp_line_comment comment

let token (t : string Wrap.t) : document =
  let prefix = pp_comments t#comments ^/^ string t#payload
  in pp_line_comment_opt prefix t#line_comment

(* Enclosed documents *)

let pp_enclosed_document
    state ?(force_hardline : bool option) (thread : document)
    break_size left right =
  let left  = token left
  and right = token right in
  group (
    match force_hardline with
      None | Some false ->
        nest state#indent (left ^^ break break_size ^^ thread)
        ^^ break break_size ^^ right
    | Some true ->
        nest state#indent (left ^^ hardline ^^ thread)
        ^^ hardline ^^ right)
        type cst                = CST.t
        type expr               = CST.expr
        type type_expr          = CST.type_expr
        type pattern            = CST.pattern
        (* type toplevel_statement = CST.toplevel_statement *)
        
        let print                    = assert false
        let print_expr               = assert false
        let print_type_expr          = assert false
        let print_pattern            = assert false