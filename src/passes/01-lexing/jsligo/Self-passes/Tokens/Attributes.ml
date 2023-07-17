(* Making attributes *)

(* Vendor dependencies *)

module Std           = Simple_utils.Std
module Region        = Simple_utils.Region
module Lexbuf        = Simple_utils.Lexbuf
module Config        = Preprocessing_jsligo.Config
module PreprocParams = Preprocessor.CLI.MakeDefault (Config)
module Parameters    = LexerLib.CLI.MakeDefault (PreprocParams)
module Options       = Parameters.Options
module Lexer         = Lexing_shared.Lexer.Make (Options) (Token)

let scan_comment scan comment region =
  let lexbuf = Lexing.from_string comment in
  let ()     = Lexbuf.reset_file region#file lexbuf in
  let line   = region#start#line in
  let ()     = Lexbuf.reset_line line lexbuf
  in scan lexbuf

let collect_attributes tokens =
  let open! Token
  in
  let rec inner acc = function
    LineCom c as com_token :: tokens -> (
      let comment = "// " ^ c#payload in
      let line_comment = Lexer.line_comment_attr com_token acc in
      match scan_comment line_comment comment c#region with
        Ok acc    -> inner acc tokens
      | Error msg -> Error (acc, msg))

  | BlockCom c as com_token :: tokens -> (
      let comment = Printf.sprintf "/* %s */" c#payload in
      let block_comment = Lexer.block_comment_attr com_token acc in
      match scan_comment block_comment comment c#region with
        Ok acc    -> inner acc tokens
      | Error msg -> Error (acc, msg))

  | Ident id as token :: tokens -> (
      match id#payload with
        "@entry" ->
          let attr = Token.mk_attr ~key:"entry" id#region
          in inner (attr :: acc) tokens
      | "@inline" ->
          let attr = Token.mk_attr ~key:"inline" id#region
          in inner (attr :: acc) tokens
      | "@view" ->
          let attr = Token.mk_attr ~key:"view" id#region
          in inner (attr :: acc) tokens
      | "@no_mutation" ->
          let attr = Token.mk_attr ~key:"no_mutation" id#region
          in inner (attr :: acc) tokens
      | "@private" ->
          let attr = Token.mk_attr ~key:"private" id#region
          in inner (attr :: acc) tokens
      | "@public" ->
          let attr = Token.mk_attr ~key:"public" id#region
          in inner (attr :: acc) tokens
      | "@hidden" ->
          let attr = Token.mk_attr ~key:"hidden" id#region
          in inner (attr :: acc) tokens
      | "@thunk" ->
          let attr = Token.mk_attr ~key:"thunk" id#region
          in inner (attr :: acc) tokens
      | "@annot" -> (
          match tokens with
            LPAR _ :: String value :: RPAR _ :: rest ->
              let value = Attr.String value#payload in
              let attr = Token.mk_attr ~key:"annot" ~value id#region
              in inner (attr :: acc) rest
          | _ -> inner (token :: acc) tokens)
      | "@layout" -> (
          match tokens with
            LPAR _ :: String value :: RPAR _ :: rest ->
              let value = Attr.String value#payload in
              let attr = Token.mk_attr ~key:"layout" ~value id#region
              in inner (attr :: acc) rest
          | _ -> inner (token :: acc) tokens)
      | _ -> inner (token :: acc) tokens)

  | token :: tokens -> inner (token :: acc) tokens
  | [] -> Ok (List.rev acc)
  in
  inner [] tokens

(* Exported *)

type message = string Region.reg

let filter :
      ?print_passes:Std.t ->
      add_warning:(Main_warnings.all -> unit) ->
      Token.t list ->
      (Token.t list, Token.t list * message) result =
  fun ?print_passes ~add_warning:_ tokens -> (* No warning registered *)
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
             "Running JsLIGO token self-pass: \
              Extraction of attributes from comments.")
    | None -> ()
  in collect_attributes tokens
