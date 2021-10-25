(* This module implements a filter on the lexical units of CameLIGO
   and produces tokens to be consumed by the parser. *)

(* Vendor dependencies *)

module Core   = LexerLib.Core
module Markup = LexerLib.Markup
module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
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


(* Exported *)

let filter = Utils.(AttachComments.attach <@ Style.check)
