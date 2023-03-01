open Simple_utils
open LexerLib
module Region = Simple_utils.Region

module type S =
  sig


    (* Lexical units *)

    type token = Token.t
    type lex_unit = token Unit.t

    (* Utility types *)

    type file_path = string
    type message   = string Region.reg

    type units = lex_unit list

    (* LEXER INSTANCE (see README.md) *)

    (* Errors *)

    type error = {
      used_units : units;
      message    : message
    }

    val read_units : file:string -> Lexing.lexbuf -> (units, error) result
    (* Instances *)

    type instance = {
      input      : Lexbuf.input;
      lexbuf     : Lexing.lexbuf;
      close      : Lexbuf.close
    }

    val open_stream : Lexbuf.input -> (instance, message) result


    type lexer =
      token State.t ->
      Lexing.lexbuf ->
      (token * token State.t, message) Stdlib.result

    val mk_string : Thread.t -> token
    val mk_eof    : Region.t -> token
    val callback  : lexer

    (* For JsLIGO only. First argument (accumulator) is the list of
       previous tokens in reverse order. *)

    val line_comment_attr :
      token list ->
      Lexing.lexbuf ->
      (token list, message) Stdlib.result

    val block_comment_attr :
      token list ->
      Lexing.lexbuf ->
      (token list, message) Stdlib.result




    type 'src lexer_generic = 'src -> (units, error) result

    (* Lexing from various sources. The default file name is [""]. *)

    val from_lexbuf  : ?file:string -> Lexing.lexbuf lexer_generic
    val from_channel : ?file:string ->    In_channel.t lexer_generic
    val from_string  : ?file:string ->        string lexer_generic
    val from_buffer  : ?file:string ->      Buffer.t lexer_generic
    val from_file    : string lexer_generic


  end
