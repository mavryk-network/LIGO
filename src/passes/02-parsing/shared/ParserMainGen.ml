(* This module is a wrapper for running the LIGO parsers as standalone
   pieces of software. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module PreprocMainGen = Preprocessor.PreprocMainGen

(* Internal dependencies *)

module type FILE        = Preprocessing_shared.File.S
module type COMMENTS    = Preprocessing_shared.Comments.S
module type TOKEN       = Lexing_shared.Token.S
module type SELF_TOKENS = Lexing_shared.Self_tokens.S
module type PARSER      = ParserLib.API.PARSER

module LexerMainGen = Lexing_shared.LexerMainGen

(* The functor *)

module type PRINTER =
  sig
    type tree
    type state

    val mk_state :
      offsets:bool -> mode:[`Point|`Byte] -> buffer:Buffer.t -> state

    val print_tokens : state -> tree -> unit
    val pp_cst       : state -> tree -> unit
  end

module type PRETTY =
  sig
    type tree
    val print : tree -> PPrint.document
  end

type 'token window = <
  last_token    : 'token option;
  current_token : 'token           (* Including EOF *)
>

module Make
         (File        : FILE)
         (Comments    : COMMENTS)
         (Token       : TOKEN)
         (ParErr      : sig val message : int -> string end)
         (Self_tokens : SELF_TOKENS with type token = Token.t)
         (CST         : sig type t end)
         (Parser      : PARSER with type token = Self_tokens.token_next
                                and type tree = CST.t)
         (Printer     : PRINTER with type tree = CST.t)
         (Pretty      : PRETTY with type tree = CST.t)
         (CLI         : ParserLib.CLI.S)
 =
  struct
    (* Instantiating the lexer *)

    module Lexer_CLI = CLI.Lexer_CLI

    module MainLexer =
      LexerMainGen.Make (File) (Token)
                        (Lexer_CLI : LexerLib.CLI.S)
                        (Self_tokens)
    (* Other CLIs *)

    module Preprocessor_CLI = Lexer_CLI.Preprocessor_CLI

    (* All exits *)

    let print_in_red msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

    let red_exit msg = print_in_red msg; exit 1

    let cli_error msg =
      red_exit (Printf.sprintf "Command-line error: %s\n" msg)

    let print_and_quit msg = print_string msg; flush stdout; exit 0

    (* Checking for errors and valid exits *)

    let check_cli () =
      MainLexer.check_cli ();
      match CLI.status with
        `SyntaxError  msg
      | `FileNotFound msg -> cli_error msg
      | `Help         buf
      | `CLI          buf -> print_and_quit (Buffer.contents buf)
      | `Version      ver -> print_and_quit (ver ^ "\n")
      | `Conflict (o1,o2) ->
           cli_error (Printf.sprintf "Choose either %s or %s." o1 o2)
      | `Done ->
           match Preprocessor_CLI.extension with
             Some ext when ext <> File.extension ->
               let msg =
                 Printf.sprintf "Expected extension %s." File.extension
               in cli_error msg
      | _ -> ()

    (* Main *)

    module ToExtRegion = 
      struct
        module Token = struct
          type token = Self_tokens.token_next
          type t = token

          let to_lexeme a = Token.to_lexeme (Self_tokens.to_token a)
          let to_string ~offsets b t =
            Token.to_string ~offsets b (Self_tokens.to_token t)
          let to_region a = Token.to_region (Self_tokens.to_token a)
          let is_eof a = Token.is_eof (Self_tokens.to_token a)
          
          let eof a     = Self_tokens.to_token_next (Token.eof a)
        end
        type token = Token.t
        
        type message = string Region.reg

        let scan a = 
          match MainLexer.scan a with 
            Ok a -> Ok (Self_tokens.to_token_next a)
          | Error e -> Error e

        type window = <
          last_token    : token option;
          current_token : token           (* Including EOF *)
        >

        let get_window a = 
          let w = MainLexer.get_window a in 
          match w with 
            Some w -> 
              Some (object
                method last_token = (match w#last_token with 
                  Some s -> 
                    Some (Self_tokens.to_token_next s) 
                | None -> None)
                method current_token = Self_tokens.to_token_next w#current_token
              end)
          | None -> 
              None
      end

    module ExtToken = 
      struct
        type t = Self_tokens.token_next
        type message = string Region.reg
        let scan a =
          match MainLexer.scan a with 
            Ok a -> 
              Ok (Self_tokens.to_token_next a)
          | Error e -> 
            Error e
      end 

    module MainParser = ParserLib.API.Make (ToExtRegion) (ExtToken) (Parser)

    let wrap : (Parser.tree, MainParser.message) result -> unit =
      function
        Stdlib.Ok tree ->
          if CLI.pretty then
            let doc = Pretty.print tree in
            let width =
              match Terminal_size.get_columns () with
                None -> 60
              | Some c -> c in
            begin
              PPrint.ToChannel.pretty 1.0 width stdout doc;
              print_newline ()
            end
          else
            let buffer = Buffer.create 231 in
            let state  = Printer.mk_state
                           ~offsets:Preprocessor_CLI.offsets
                           ~mode:Lexer_CLI.mode
                           ~buffer in
            if CLI.cst then
              begin
                Printer.pp_cst state tree;
                Printf.printf "%s%!" (Buffer.contents buffer)
              end
            else
              if CLI.cst_tokens then
                begin
                  Printer.print_tokens state tree;
                  Printf.printf "%s%!" (Buffer.contents buffer);
                end
              else ();
            flush_all ()
      | Error Region.{value; region} ->
         let reg = region#to_string ~file:true ~offsets:true `Point in
         let msg = Printf.sprintf "Parse error %s:\n%s" reg value
         in (flush_all (); print_in_red msg)

    let config =
      object
        method offsets = Preprocessor_CLI.offsets
        method mode    = Lexer_CLI.mode
      end

    module Preproc = PreprocMainGen.Make (Preprocessor_CLI)

    let parse () =
      if Lexer_CLI.preprocess then
        match Preproc.preprocess () with
          Stdlib.Error _ -> ()
        | Stdlib.Ok (buffer, _deps) ->
            if Preprocessor_CLI.show_pp then
              Printf.printf "%s%!" (Buffer.contents buffer)
            else ();
            let string = Buffer.contents buffer in
            let lexbuf = Lexing.from_string string in
            let open MainParser in
            if CLI.mono then
              mono_from_lexbuf lexbuf |> wrap
            else
              incr_from_lexbuf (module ParErr) lexbuf |> wrap
      else
        let open MainParser in
        match Preprocessor_CLI.input with
          None ->
            if CLI.mono then
              mono_from_channel stdin |> wrap
            else
              incr_from_channel (module ParErr) stdin |> wrap
        | Some file_path ->
            if CLI.mono then
              mono_from_file file_path |> wrap
            else
              incr_from_file (module ParErr) file_path |> wrap
  end
