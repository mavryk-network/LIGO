(* This module implements a filter on the lexical units of PascaLIGO
   and produces tokens to be consumed by the parser. *)

(* Vendor dependencies *)

module Core   = LexerLib.Core
module Markup = LexerLib.Markup
module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils

(* Signature *)

module type S =
  sig
    type token
    type lex_unit = token Core.lex_unit

    type message = string Region.reg

    val filter :
      (lex_unit list, message) result -> (token list * Markup.t list, message) result
  end

(* Filters *)

let (let*) = Result.bind
let ok     = Result.ok

type message = string Region.reg

type token = Token.t
type lex_unit = token Core.lex_unit

(* Filtering out the markup *)

let tokens_of lex_units =
  let apply (tokens, comments) = function
    Core.Token token -> (token::tokens, comments)
  | Markup (LineCom _ as l) -> (tokens, l :: comments)
  | Markup (BlockCom _ as l) -> (tokens, l :: comments)
  | Markup    _ -> (tokens, comments)
  | Directive d -> (Token.Directive d :: tokens, comments)
  in List.fold_left apply ([], []) lex_units |> (fun (a, b) -> (List.rev a, List.rev b)) |> ok

(* Exported *)

let filter tokens = 
  let* tokens = Style.check tokens in
  let* (tokens, comments) = tokens_of tokens in
  ok (tokens, comments)