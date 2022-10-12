(* Driver for the CameLIGO parser *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std
module Lexbuf = Simple_utils.Lexbuf

(* Internal dependencies *)

module Config      = Preprocessing_cameligo.Config
module Token       = Lexing_cameligo.Token
module UnitPasses  = Lexing_cameligo_self_units.Self
module TokenPasses = Lexing_cameligo_self_tokens.Self
module ParErr      = Parsing_cameligo.ParErr
module Tree        = Cst_shared.Tree
module CST         = Cst_cameligo.CST

(* APIs *)

module PreprocAPI = Preprocessor.TopAPI
module LexerAPI   = Lexing_shared.TopAPI
module ParserAPI  = Parsing_shared.TopAPI

(* CLIs *)

module PreprocParams = Preprocessor.CLI.Make (Config)
module LexerParams   = LexerLib.CLI.Make (PreprocParams)
module Parameters    = ParserLib.CLI.Make (LexerParams)

(* Instantiating preprocessor and lexer *)

module Preprocessor = PreprocAPI.Make (PreprocParams)

module Warning =
  struct
    let add _ = () (* No warning registered *)
  end

module Lexer =
  LexerAPI.Make
    (Preprocessor)
    (LexerParams)
    (Token)
    (UnitPasses)
    (TokenPasses)
    (Warning)

(* Renamings on the parser generated by Menhir to suit the functor. *)

module Parser =
  struct
    include Parsing_cameligo.Parser
    type tree = CST.t

    let main = contract

    module Incremental =
      struct
        let main = Incremental.contract
      end

    module Recovery = Parsing_cameligo.RecoverParser
  end

module Pretty =
  struct
    include Parsing_cameligo.Pretty
    type tree = CST.t

  end

module Print =
  struct
    type tree = CST.t
    type state = Tree.state
    let mk_state = Tree.mk_state
    include Cst_cameligo.Print
  end

(* Finally... *)

module Main =
  ParserAPI.Make
    (Preprocessor)
    (Lexer)
    (Parameters)
    (ParErr)
    (Warning)
    (UnitPasses)
    (TokenPasses)
    (CST)
    (Parser)
    (Print)
    (Pretty)

let () =
  let open! Main in
  match check_cli () with
    Ok ->
      let file = Option.value Parameters.Options.input ~default:"" in
      let std, _cst = parse (Lexbuf.File file) in
      let () = Std.(add_nl std.out) in
      let () = Std.(add_nl std.err) in
      Printf.printf  "%s%!" (Std.string_of std.out);
      Printf.eprintf "%s%!" (Std.string_of std.err)
  | Error msg -> Printf.eprintf "%s\n%!" msg
  | Info  msg -> Printf.printf "%s%!" msg (* Note the absence of "\n" *)
