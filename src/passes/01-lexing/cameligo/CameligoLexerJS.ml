open Js_of_ocaml
(* Driver for the Cameligo lexer *)

(* Local dependencies *)

module Config         = Preprocessing_cameligo.Config
module PreprocParams  = Preprocessor.CLI.Make (Config)
module Token          = Lexing_cameligo.Token
module UnitPasses     = Lx_ml_self_units.Self
module TokenPasses    = Lx_ml_self_tokens.Self

(* Vendors dependencies *)

module Std        = Simple_utils.Std
module Lexbuf     = Simple_utils.Lexbuf
module Parameters = LexerLib.CLI.Make (PreprocParams)
module PreprocAPI = Preprocessor.TopAPI.Make (PreprocParams)

module Warning =
  struct
    let add _ = () (* No warning registered *)
  end

module API =
  Lexing_shared.TopAPI.Make
    (PreprocAPI) (Parameters) (Token)
    (UnitPasses) (TokenPasses) (Warning)

open API

let main source =
  let (std, tokens) = scan_all_tokens ~no_colour:true source in
  let () = Std.(add_nl std.out) in
  let () = Std.(add_nl std.err) in
  Printf.printf  "%s%!" (Std.string_of std.out);
  Printf.eprintf "%s%!" (Std.string_of std.err);
  tokens


let _ =
  Js.export
    "ligoML"
    (object%js
       method cameligoLexer code =
         let code = Js.to_string code in
         match main (String ("foo.mligo", code)) with
         | Ok tokens -> tokens |> List.map Token.to_lexeme |> List.iter print_endline
         | Error e -> print_endline "err"
    end)
