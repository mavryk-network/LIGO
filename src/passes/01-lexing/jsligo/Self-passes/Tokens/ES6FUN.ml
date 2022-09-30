(* This module implements a filter on the lexical units of JsLIGO
   and produces tokens to be consumed by the parser. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std
module Unit   = LexerLib.Unit

(* LIGO's dependencies *)

module Wrap = Lexing_shared.Wrap

(* Utility modules and types *)

module List = Core.List

(* Filters *)

let ok x = Stdlib.Ok x

type message = string Region.reg

type token = Token.t

(* It's not a lexer *)
(*
type window = <
  last_token    : token option;
  current_token : token           (* Including EOF *)
>

let window : window option ref = ref None

let set_window ~current ~last : unit =
  window := Some (object
                    method last_token    = last
                    method current_token = current
                  end)
 *)

let fake_lexer : token list -> Lexing.lexbuf -> token =
  fun tokens ->
    let store = ref tokens in
    fun _ ->
      match !store with
        token::tokens ->
(*          let last =
            match !window with
              None -> None
            | Some window -> Some window#current_token in
          set_window ~current:token ~last; *)
          store := tokens;
          token
      | [] -> Token.ghost_EOF

let pre_parser tokens : _ result =
  let fake_lexer   = fake_lexer tokens in
  let fake_lexbuf  = Lexing.from_string "" in
  let module Inter = PreParser.MenhirInterpreter in
  let supplier     = Inter.lexer_lexbuf_to_supplier
                       fake_lexer fake_lexbuf in
  let success a    = ok a
  in
  let failure = function
    Inter.Accepted s ->  Ok s
  | HandlingError _env ->
      (* We don't handle this here, but let it go through *)
      Error ([], Region.wrap_ghost "Parse error.")
  | _ -> Error ([], Region.wrap_ghost "Unhandled state.")
  in
  let checkpoint = PreParser.Incremental.self_pass
                     fake_lexbuf.lex_curr_p in
  let _buffer, supplier =
    MenhirLib.ErrorReports.wrap_supplier supplier in
  Inter.loop_handle success failure supplier checkpoint

(* Debug *)

let print_token token =
  Printf.printf "%s\n" (Token.to_string ~offsets:true `Point token)

let apply filter = function
  Stdlib.Ok tokens -> filter tokens
| Error _ as err   -> err

let print_tokens (tokens: (token list, _) result) =
  apply (fun tokens -> List.iter ~f:print_token tokens; Ok tokens) tokens

(* Exported *)

let filter
    : ?print_passes:Std.t ->
      add_warning:(Main_warnings.all -> unit) ->
      token list ->
      _ result =
  fun ?print_passes ~add_warning:_ tokens -> (* No warning registered *)
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
             "Running JsLIGO token self-pass: \
              Injecting ES6FUN virtual tokens.")
    | None -> ()
  in pre_parser tokens
