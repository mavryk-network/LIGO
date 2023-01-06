(* Making parsers from a variety of input using Menhir. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils
module Unit   = LexerLib.Unit

(* Generic signature of tokens *)

module type TOKEN =
  sig
    type token
    type t = token

    val to_lexeme : token -> string
    val to_region : token -> Region.t
    val is_eof    : token -> bool
  end

(* Generic signature of input lexers *)

module type LEXER =
  sig
    module Token : TOKEN
    type token = Token.t

    type message = string Region.reg

    val scan_token : no_colour:bool -> Lexing.lexbuf -> (token, message) result

    val used_tokens : unit -> token list (* Scanned tokens *)

    val clear : unit -> unit
  end

(* The signature generated by Menhir with an additional type
   definition for [tree]. *)

module type PARSER =
  sig
    type token
    type tree

    (* The monolithic API. *)

    exception Error

    val main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> tree

    (* The incremental API. *)

    module MenhirInterpreter : MenhirLib.IncrementalEngine.EVERYTHING
           with type token = token

    module Incremental :
      sig
        val main : Lexing.position -> tree MenhirInterpreter.checkpoint
      end

    (* The recovery API. *)

    module Recovery :
      sig
        include Merlin_recovery.RECOVERY_GENERATED
                with module I := MenhirInterpreter

        val default_value : Region.t -> 'a MenhirInterpreter.symbol -> 'a
      end
  end

(* Mappimg from error states in the LR automaton generated by Menhir
   to error messages (incremental API of Menhir) *)

module type PAR_ERR =
  sig
    val message : int -> string
  end

(* Debug setting *)

module type DEBUG_CONFIG =
  sig
    (* Assume that positions refer to bytes or code points. It mostly
       affects the position of synthesized tokens in the error
       recovery mode because menhir requires to convert [Pos.t] type
       to the poorer representation [Lexing.position]. *)

    val mode : [`Byte | `Point]

    (* Enable debug printing in the recovery algorithm. The argument
       is the path to a log file.
         * [None] means no trace recovery;
         * [Some None] means to use stdout;
         * [Some (Some path)] means to use file [path]. *)

    val trace_recovery : string option option
  end

(* The functor integrating the parser with its errors *)

module Make (Lexer  : LEXER)
            (Parser : PARSER with type token = Lexer.Token.t)
            (Debug  : DEBUG_CONFIG) :
  sig
    type token = Lexer.Token.t

    type message = string Region.reg

    type error = {
      used_tokens : token list;
      message     : message
    }

    type pass_error =
      Parsing of error
    | Lexing  of error
    | System  of error

    type 'src parser = 'src -> (Parser.tree, pass_error) Stdlib.result

    (* Monolithic API of Menhir *)

    type file_path = string

    val mono_from_lexbuf  : no_colour:bool -> Lexing.lexbuf parser
    val mono_from_channel : no_colour:bool -> in_channel    parser
    val mono_from_string  : no_colour:bool -> string        parser
    val mono_from_file    : no_colour:bool -> file_path     parser

    (* Incremental API of Menhir without recovery on error *)

    val incr_from_lexbuf  : no_colour:bool -> (module PAR_ERR) -> Lexing.lexbuf parser
    val incr_from_channel : no_colour:bool -> (module PAR_ERR) -> in_channel    parser
    val incr_from_string  : no_colour:bool -> (module PAR_ERR) -> string        parser
    val incr_from_file    : no_colour:bool -> (module PAR_ERR) -> file_path     parser

    (* Incremental API with recovery *)

    (* The type ['src recovery_parser] denotes parsers with recovery
       on error. The results are one of the following:

         * [Ok (tree, [])] if the input of type ['src] contains a
           syntactically valid contract;

         * [Ok (repaired_tree, errors)] in case of syntax errors;

         * [Error errors] for non-syntactical errors, e.g. the input
           is not found or a lexer error occurred. *)

    type 'src recovery_parser =
      'src -> (Parser.tree * message list, message Utils.nseq) Stdlib.result

    (* Parsing with recovery from various sources *)

    val recov_from_lexbuf  : no_colour:bool -> (module PAR_ERR) -> Lexing.lexbuf recovery_parser
    val recov_from_channel : no_colour:bool -> (module PAR_ERR) -> in_channel    recovery_parser
    val recov_from_string  : no_colour:bool -> (module PAR_ERR) -> string        recovery_parser
    val recov_from_file    : no_colour:bool -> (module PAR_ERR) -> file_path     recovery_parser

    (* Formatting syntax error messages *)

    val format_error : no_colour:bool -> file:bool -> string -> Region.t -> string Region.reg
  end