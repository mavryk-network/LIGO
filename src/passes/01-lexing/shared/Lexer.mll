(* Lexer specification for LIGO, to be processed by [ocamllex].

   The underlying design principles are:

     (1) provide precise error messages with hints as how to fix the
         issue, which is achieved by consulting the lexical
         right-context of lexemes;

     (2) be as independent as possible from the LIGO version, so
         upgrades have as little impact as possible on this
         specification: this is achieved by using the most general
         regular expressions to match the lexing buffer and broadly
         distinguish the syntactic categories, and then delegating a
         finer, second analysis to an external module making the
         tokens (hence a functor below);

     (3) support unit testing (lexing of the whole input with debug
         traces).

     A limitation to the independence with respect to the LIGO version
   lies in the errors that the external module building the tokens
   (which may be version-dependent) may have to report. Indeed these
   errors have to be contextualised by the lexer in terms of input
   source regions, so useful error messages can be printed, therefore
   they are part of the signature [Token.S] that parameterises the
   functor generated here. For instance, if, in a future release of
   LIGO, new tokens are added, and the recognition of their lexemes
   entails new errors, the signature [Token.S] will have to be
   augmented and this lexer specification changed. However, in
   practice, it is more likely that instructions or types will be
   added, instead of new kinds of tokens. *)

{
[@@@warning "-42"]

(* OCaml Stdlib *)

module Array = Caml.Array (* Used in the generated code only *)
module Int64 = Caml.Int64

(* VENDOR DEPENDENCIES *)

module Region    = Simple_utils.Region
module Lexbuf    = Simple_utils.Lexbuf
module Options   = LexerLib.Options   (* For instantiation only *)
module Unit      = LexerLib.Unit      (* For instantiation only *)
module Directive = Preprocessor.Directive (* For verbatim only  *)
module State     = LexerLib.State
module Thread    = LexerLib.Thread
module Pos       = Simple_utils.Pos
module PreprocessorConfig    = Preprocessor.Config


(* START CORE HEADER *)

(* Vendor dependencies *)

(* Third-party libraries *)

(* UTILITIES *)

let (let*) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result =
  fun r f ->
    match r with
      Ok x         -> f x
    | Error _ as e -> e


(* THE FUNCTOR *)

(*****************************************************************************************)
(* We don't have Core.Make, LowAPI.Make and Lexer.Make separately anymore. To merge      *)
(* Lexer.mll and Core.mll, these functors had to be merged into one - Lexer.Make.        *)
(* In the process, of course, the functor param Client disappears as it gets amalgamated *)
(* into the new monolithic Lexer.Make                                                    *)
(*****************************************************************************************)

module Make (Config : PreprocessorConfig.S) (Options : Options.S) (Token : Token.S) =
  struct
    exception DontScan of Token.t State.t
    exception ScanOneMoreTime of Token.t State.t * Token.t State.t

    type lex_unit = Token.t LexerLib.Unit.t

    type units = lex_unit list

    (* Errors *)

    type message = string Region.reg

    type error = {
      used_units : units;
      message    : message
    }

    type token = Token.t

    (* ERRORS *)

    type dialect_error =
      Unexpected_character of char
    | Non_canonical_zero
    | Invalid_symbol of string
    | Wrong_nat_syntax of string
    | Wrong_mutez_syntax of string
    | Wrong_lang_syntax of string
    | Unterminated_verbatim of string
    | Overflow_mutez
    | Underflow_mutez
    | Invalid_directive of Preprocessor.Error.t
    | Unterminated_comment

    let sprintf = Printf.sprintf

    let dialect_error_to_string = function
      Unexpected_character c ->
        sprintf "Unexpected character '%s'." (Char.escaped c)
    | Non_canonical_zero ->
        "Non-canonical zero.\n\
         Hint: Use 0."
    | Invalid_symbol s ->
        sprintf "Invalid symbol: %S.\n\
                 Hint: Check the LIGO syntax you use." s
    | Wrong_nat_syntax hint ->
        sprintf "Wrong nat syntax.\n%s" hint
    | Wrong_mutez_syntax hint ->
        sprintf "Wrong mutez syntax.\n%s" hint
    | Wrong_lang_syntax hint ->
        sprintf "Wrong code injection syntax.\n%s" hint
    | Unterminated_verbatim term ->
        sprintf "Unterminated verbatim.\n\
                 Hint: Close with %S." term
    | Overflow_mutez ->
        "Mutez amount too large.\n\
         Note: From 0 to 2^63-1=9_223_372_036_854_775_807."
    | Underflow_mutez ->
        "Mutez amount not an integer."
    | Invalid_directive err ->
        Preprocessor.Error.to_string err
    | Unterminated_comment ->
       sprintf "Unterminated comment.\n\
                Note: Check any ill-formed attribute."

    (* Failure *)

    let fail state region error =
      let value      = LexerLib.Error.to_string error in
      let message    = Region.{value; region} in
      let used_units = List.rev state#lexical_units
      in Stdlib.Error {used_units; message}

    let mk_eof = Token.mk_eof (* For EOFs from the preprocessor *)
    let mk_string thread =
      let start  = thread#opening#start in
      let stop   = thread#closing#stop in
      let region = Region.make ~start ~stop in
      Token.mk_string (thread#to_string) region

    (* Auxiliary functions for preprocessing directives *)

    let handle_ending state lexbuf = function
      `EOL (state', ending) ->
         (state#push_newline (Some ending) lexbuf)#set_pos state'#pos
    | `EOF (state', region) ->
         (state#push_token (mk_eof region))#set_pos state'#pos

    let scan_dir ~callback scan (state: Token.t LexerLib.State.t) lexbuf =
      let state, Region.{region; _} = state#sync lexbuf in
      let state' = new Preprocessor.State.t state#pos in
      match scan region#start state' lexbuf with
        Stdlib.Error (region, err) ->
          fail state region (LexerLib.Error.Invalid_directive err)
      | Ok (state', _, dir, ending) ->
          let state = state#set_pos state'#pos in
          let state = state#push_directive dir in
          let state = handle_ending state lexbuf ending
          in callback state lexbuf

    let scan_if   = scan_dir
    let scan_elif = scan_dir

    let scan_else ~callback scan state lexbuf =
      let state, Region.{region; _} = state#sync lexbuf in
      let state' = new Preprocessor.State.t state#pos in
      let _, dir, ending = scan region#start state' lexbuf in
      let state = state#push_directive dir in
      let state = handle_ending state lexbuf ending
      in callback state lexbuf

    let scan_endif = scan_else

    let scan_linemarker ~callback linenum state lexbuf =
      (* We save the state and position before the lexing buffer was
         matched. *)

      let hash_state = state in

      (* We syncronise the logical state with the matched string *)

      let state, Region.{region; _} = state#sync lexbuf in

      (* We determine the regon of the line number *)

      let length   = String.length linenum in
      let start    = region#stop#shift_bytes (-length) in
      let line_reg = Region.make ~start ~stop:region#stop in
      let linenum  = Region.{region=line_reg; value=linenum} in


      (* We make a preprocessing state and scan the expected
         linemarker. *)

      let preproc_state = new Preprocessor.State.t state#pos in

      match Directive.scan_linemarker
              hash_state#pos linenum preproc_state lexbuf
      with
        Stdlib.Error (region, error) ->
          fail hash_state region (LexerLib.Error.Invalid_directive error)

      | Ok (preproc_state, args, directive, _ending) ->
          (* We use the current position (after reading the linemarker)
             of the preprocessing state [preproc_state] to reset the
             position of saved lexing state [hash_state]. (Remember that
             positions contain the file name.) We push the linemarker in
             that updated lexing state. *)

          let state = hash_state#set_pos preproc_state#pos in
          let state = state#push_directive directive in
          let state = state#push_newline None lexbuf in

          let arg_file = args#file_path.Region.value in

          let push state =
            let pos   = state#pos in
            let pos   = pos#set_file arg_file in
            let pos   = pos#set_line args#linenum.Region.value in
            let pos   = pos#reset_cnum in
            let state = state#set_pos pos in
            let ()    = Lexbuf.reset_file arg_file lexbuf
            in state in

          match args#flag with
            Some Region.{value=Directive.Pop; _} ->
              (* The linemarker has been produced by the end of the
                 preprocessing of an #include directive. We assume that
                 the user never writes one, otherwise preprocessing
                 may fail. More precisely, we assume that each [Push]
                 (below) is associate to one [Pop] (this case). *)
              Lexbuf.reset_file arg_file lexbuf;
             raise @@ DontScan state

          | None ->
              (* The linemarker is the one generated at the start of
                 the file or one written by the user. *)
             let state = push state in
             Ok state

          | Some Region.{value=Directive.Push; _} ->
             let state = push state in
             raise @@ ScanOneMoreTime (state, hash_state)



    (* The lexer instance: the main exported data type *)

    type file_path = string

    type instance = {
      input      : Lexbuf.input;
      read_units : Lexing.lexbuf -> (units, error) result;
      lexbuf     : Lexing.lexbuf;
      close      : Lexbuf.close
    }

    (* The main function *)

    let open_stream scan input : (instance, message) result =
      let file = Lexbuf.file_from_input input in
      let read_units lexbuf =
        let state  = LexerLib.State.empty ~file in
        let* state = scan state lexbuf in
        Ok (List.rev state#lexical_units) in
      let* lexbuf, close = Lexbuf.from_input input
      in Ok {read_units; input; lexbuf; close}

    (* Reading UTF-8 encoded characters *)

    let scan_utf8_wrap scan_utf8 callback thread state lexbuf =
      let ()  = Lexbuf.rollback lexbuf in
      let len = thread#length in
      match scan_utf8 thread state lexbuf with
        Stdlib.Ok (thread, state) ->
          let delta = thread#length - len in
          let stop  = state#pos#shift_one_uchar delta
          in callback thread (state#set_pos stop) lexbuf
      | Error (thread, state, error) ->
          let delta  = thread#length - len in
          let stop   = state#pos#shift_one_uchar delta in
          let region = Region.make ~start:state#pos ~stop
          in fail state region error

    let open_block thread state =
      Stdlib.Error (thread, state, LexerLib.Error.Unterminated_comment)

(* END CORE HEADER *)

(* Lexer specification for all LIGO dialects *)
(* START DIALECT-SPECIFIC HEADER *)

    (* Raising the exception for lexical errors *)

    exception Error of message

    let fail_with_region (region: Region.t) error =
      let msg = dialect_error_to_string error in
      raise (Error Region.{value=msg; region})

    (* TOKENS *)

    (* Strings *)

    (* Verbatim strings *)

    let mk_verbatim (thread, state) =
      let start  = thread#opening#start in
      let stop   = state#pos in
      let region = Region.make ~start ~stop in
      let lexeme = thread#to_string in
      let token  = Token.mk_verbatim lexeme region
      in token, state

    (* Bytes *)

    let mk_bytes bytes state buffer =
      let state, Region.{region; _} = state#sync buffer in
      let norm  = Str.(global_replace (regexp "_") "" bytes) in
      let token = Token.mk_bytes bytes norm region
      in token, state

    (* Integers *)

    let mk_int state buffer =
      let state, Region.{region; value} = state#sync buffer in
      let lexeme = value in
      let z = Z.of_string lexeme in
      if   Z.equal z Z.zero && String.(lexeme <> "0")
      then fail_with_region region Non_canonical_zero
      else let token = Token.mk_int lexeme z region
           in token, state

    (* Natural numbers *)

    let mk_nat nat state buffer =
      let state, Region.{region; _} = state#sync buffer
      and z = Z.of_string nat in
      if   Z.equal z Z.zero && String.(nat <> "0")
      then fail_with_region region Non_canonical_zero
      else match Token.mk_nat nat z region with
             Ok token -> token, state
           | Error Token.Wrong_nat_syntax hint ->
               fail_with_region region (Wrong_nat_syntax hint)

    (* Mutez *)

    let mk_mutez nat state buffer =
      let state, Region.{region; _} = state#sync buffer in
      match Int64.of_string_opt nat with
        None -> fail_with_region region Overflow_mutez
      | Some mutez_64 ->
          if   Int64.equal mutez_64 Int64.zero && String.(nat <> "0")
          then fail_with_region region Non_canonical_zero
          else let suffix = "mutez" in
               match Token.mk_mutez nat ~suffix mutez_64 region with
                 Ok token -> token, state
               | Error Token.Wrong_mutez_syntax hint ->
                   fail_with_region region (Wrong_mutez_syntax hint)

    (* Integral Tez (internally converted to mutez) *)

    let mk_tez nat suffix state buffer =
      let state, Region.{region; _} = state#sync buffer
      and mutez = Z.mul (Z.of_int 1_000_000) (Z.of_string nat) in
      try
        let mutez_64 = Z.to_int64 mutez in
        if   Int64.equal mutez_64 Int64.zero && String.(nat <> "0")
        then fail_with_region region Non_canonical_zero
        else match Token.mk_mutez nat ~suffix mutez_64 region with
               Ok token -> token, state
             | Error Token.Wrong_mutez_syntax hint ->
                 fail_with_region region (Wrong_mutez_syntax hint)
      with Z.Overflow -> fail_with_region region Overflow_mutez

    (* Tez as a decimal number (internally converted to mutez) *)

    let mk_tez_dec integral fractional suffix state buffer =
      let state, Region.{region; _} = state#sync buffer in
      let integral'   = Str.(global_replace (regexp "_") "" integral)
      and fractional' = Str.(global_replace (regexp "_") "" fractional) in
      let numerator   = Z.of_string (integral' ^ fractional')
      and frac_length = String.length fractional' in
      let denominator = Z.of_string ("1" ^ String.make frac_length '0')
      and million     = Q.of_string "1_000_000" in
      let q_mutez     = Q.make numerator denominator |> Q.mul million in
      if Z.equal (Q.den q_mutez) Z.one then
        try
          let mutez_64 = Z.to_int64 (Q.num q_mutez) in
          if   Int64.equal mutez_64 Int64.zero
               && String.(integral <> "0" || fractional <> "0")
          then fail_with_region region Non_canonical_zero
          else let lexeme = integral ^ "." ^ fractional in
               match Token.mk_mutez lexeme ~suffix mutez_64 region with
                 Ok token -> token, state
               | Error Token.Wrong_mutez_syntax hint ->
                   fail_with_region region (Wrong_mutez_syntax hint)
        with Z.Overflow -> fail_with_region region Overflow_mutez
      else fail_with_region region Underflow_mutez

    (* Identifiers *)

    let mk_ident state buffer =
      let state, Region.{region; value} = state#sync buffer in
      let token = Token.mk_ident value region
      in token, state

    (* Attributes *)

    let mk_str_attr key ?value state buffer =
      let state, Region.{region; _} = state#sync buffer
      and value =
        match value with
          None        -> None
        | Some string -> Some (Attr.String string) in
      let token = Token.mk_attr ~key ?value region
      in token, state

    let mk_id_attr key ?value state buffer =
      let state, Region.{region; _} = state#sync buffer
      and value =
        match value with
          None      -> None
        | Some name -> Some (Attr.Ident name) in
      let token = Token.mk_attr ~key ?value region
      in token, state

    (* Data constructors and module names *)

    let mk_uident state buffer =
      let state, Region.{region; value} = state#sync buffer in
      let token = Token.mk_uident value region
      in token, state

    (* Code injection *)

     let mk_lang start lang state buffer =
      let state, Region.{region; _} = state#sync buffer in
      let start    = region#start#shift_bytes (String.length start) in
      let stop     = region#stop in
      let lang_reg = Region.make ~start ~stop in
      let lang     = Region.{value=lang; region=lang_reg} in
      match Token.mk_lang lang region with
        Ok token -> token, state
      | Error Token.Wrong_lang_syntax hint ->
          fail_with_region region (Wrong_lang_syntax hint)

    (* Symbols *)

    let mk_sym state buffer =
      let state, Region.{region; value} = state#sync buffer in
      match Token.mk_sym value region with
        Ok token -> token, state
      | Error Token.Invalid_symbol string ->
          fail_with_region region (Invalid_symbol string)

    (* End-of-File *)

    let mk_eof state buffer =
      let state, Region.{region; _} = state#sync buffer in
      let token = Token.mk_eof region
      in token, state

(***********************************************************************************)
(* callback_with_cont has been inlined                                             *)
(*                                                                                 *)
(* Note: this will duplicate some logic to fix too many stackframes                *)
(*                                                                                 *)
(* Reason being:                                                                   *)
(* From Js_of_ocaml docs,                                                          *)
(*                                                                                 *)
(* https://ocsigen.org/js_of_ocaml/latest/manual/tailcall                          *)
(*                                                                                 *)
(* Examples of pattern not optimized                                               *)
(* Recursive function where the tail call is made inside an intermediate function. *)
(*                                                                                 *)
(* let rec f x =                                                                   *)
(*   let g delta = f (x - delta) in                                                *)
(*   if x < 0 then 0                                                               *)
(*   else if x mod 2 = 0                                                           *)
(*   then g 2                                                                      *)
(*   else g 1;;                                                                    *)
(* Tail call of a function given as argument                                       *)
(*                                                                                 *)
(* let bind x f =                                                                  *)
(*   match x with                                                                  *)
(*   | None -> None                                                                *)
(*   | Some x -> f x                                                               *)
(* Note that in the future, more tail call optimizations could be perform          *)
(* with function specialization and better inlining.                               *)
(***********************************************************************************)



(* END DIALECT-SPECIFIC HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

(* LexerLib *)
(* START LEXERLIB DEFINITION *)

(* NAMED REGULAR EXPRESSIONS *)

let utf8_bom = "\xEF\xBB\xBF" (* Byte Order Mark for UTF-8 *)
let nl       = ['\n' '\r'] | "\r\n"
let blank    = ' ' | '\t'
let digit   = ['0'-'9']
let natural = digit | digit+ digit (* Linemarkers *)

(* Comment delimiters *)

let pascaligo_block_comment_opening  = "(*"
let pascaligo_block_comment_closing  = "*)"
let pascaligo_line_comment_opening   = "//"

let cameligo_block_comment_opening   = "(*"
let cameligo_block_comment_closing   = "*)"
let cameligo_line_comment_opening    = "//"

let jsligo_block_comment_opening     = "/*"
let jsligo_block_comment_closing     = "*/"
let jsligo_line_comment_opening      = "//"

let pyligo_block_comment_opening     = "/*"
let pyligo_block_comment_closing     = "*/"
let pyligo_line_comment_opening      = "##"

let block_comment_opening =
   pascaligo_block_comment_opening
|   cameligo_block_comment_opening
|     jsligo_block_comment_opening
|     pyligo_block_comment_opening

let block_comment_closing =
   pascaligo_block_comment_closing
|   cameligo_block_comment_closing
|     jsligo_block_comment_closing
|     pyligo_block_comment_closing

let line_comment_opening =
   pascaligo_line_comment_opening
|   cameligo_line_comment_opening
|     jsligo_line_comment_opening
|     pyligo_line_comment_opening

(* String delimiters *)

let  pascaligo_string_delimiter = "\""
let   cameligo_string_delimiter = "\""
let     jsligo_string_delimiter = "\""
let     pyligo_string_delimiter = "\""

let string_delimiter =
   pascaligo_string_delimiter
|   cameligo_string_delimiter
|     jsligo_string_delimiter
|     pyligo_string_delimiter

(* Preprocessing directives *)

let directive =
  "include"
| "import"
| "if"
| "elif"
| "else"
| "endif"
| "define"
| "undef"
| "error"

(* Dialect specific *)
let nl        = ['\n' '\r'] | "\r\n"
let blank     = ' ' | '\t'
let digit     = ['0'-'9']
let natural   = digit | digit (digit | '_')* digit
let nat       = natural as nat
let tz_or_tez = "tz" | "tez" as tez
let decimal   = (natural as integral) '.' (natural as fractional)
let small     = ['a'-'z']
let capital   = ['A'-'Z']
let letter    = small | capital
let ident     = (small | '_'+ (letter | digit)) (letter | '_' | digit)*
let ext_ident = '@' (letter | digit | '_')+
let uident    = capital (letter | '_' | digit)*

let string    = '"' [^ '"' '\\' '\n']* '"' as value
let key       = letter (letter | digit | '_' | '.' (letter | digit))*
let str_attr  = (key as key) ((blank* ':' blank* | blank+) (string as value))?
let id_attr   = (key as key)
                ((blank* ':' blank* | blank+) (ident | uident as value))?

let hex_digit = digit | ['A'-'F' 'a'-'f']
let byte      = hex_digit hex_digit
let byte_seq  = byte | byte (byte | '_')* byte
let bytes     = "0x" (byte_seq? as bytes)
let directive = '#' (blank* as _space) (small+ as id) (* For #include *)
let code_inj  = ("[%" as start) (key as lang)

(* Symbols *)

let     common_sym =   ";" | "," | "(" | ")"  | "[" | "]"  | "{" | "}"
                     | "=" | ":" | "|" | "." | "_"
                     | "+" | "-" | "*" | "/"  | "<" | "<=" | ">" (*| ">="*)
let  pascaligo_sym = "->" | "=/=" | "#" | ":=" | "^"
let   cameligo_sym = "->" | "<>" | "::" | "||" | "&&" | "'" | "|>" | "^"
let     jsligo_sym =   "..." | "?" | "!" | "%" | "==" | "!=" | "+=" | "-="
                     | "*=" | "/="| "%=" | "=>"
let     pyligo_sym = "->" | "^"   | "**"  | "//" | "%"  | "@"  | "|" | "&"
                   | "~"  | "`"   | "\\"
                   | "==" | "!=" | "+=" | "-="
                   | "*=" | "/="  | "//=" | "%=" | "@=" | "&=" | "|="
                   | "^=" | "<<=" | "**=" (* | ">>=" *)
let symbol =
      common_sym
|  pascaligo_sym
|   cameligo_sym
|     jsligo_sym
|     pyligo_sym

(* RULES FROM LEXERLIB (SCANNERS) *)

rule scan state = parse
  (* Markup *)

  nl    { scan (state#push_newline None lexbuf) lexbuf }
| ' '+  { scan (state#push_space        lexbuf) lexbuf }
| '\t'+ { scan (state#push_tabs         lexbuf) lexbuf }

  (* Strings *)

| string_delimiter {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.string with
      Some delimiter when String.(delimiter = lexeme) ->
        let state, Region.{region; _} = state#sync lexbuf in
        let thread = LexerLib.Thread.make ~opening:region in
        let* thread, state = in_string thread state lexbuf in
        let token = mk_string thread
        in scan (state#push_token token) lexbuf
    | Some _ | None ->

      let () = Lexbuf.rollback lexbuf in
      let* state = 
        match
          try Stdlib.Ok (dialect_scan state lexbuf) with
            Error msg -> Stdlib.Error msg
        with
          Ok (token, state) ->
           Ok (state#push_token token)
        | Error message ->
           let used_units = List.rev state#lexical_units
           in Error {used_units; message}
      in
      scan state lexbuf }

  (* Comments *)

| block_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.block with
      Some block when String.(block#opening = lexeme) ->
        let state, Region.{region; _} = state#sync lexbuf in
        let thread = LexerLib.Thread.make ~opening:region in
        let thread = thread#push_string lexeme in
        let* thread, state = in_block block thread state lexbuf
        in scan (state#push_block thread) lexbuf
    | Some _ | None ->
      let () = Lexbuf.rollback lexbuf in
      let* state = 
        match
          try Stdlib.Ok (dialect_scan state lexbuf) with
            Error msg -> Stdlib.Error msg
        with
          Ok (token, state) ->
           Ok (state#push_token token)
        | Error message ->
           let used_units = List.rev state#lexical_units
           in Error {used_units; message}
      in
      scan state lexbuf }

| line_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.line with
      Some opening when String.(opening = lexeme) ->
        let state, Region.{region; _} = state#sync lexbuf in
        let thread = LexerLib.Thread.make ~opening:region in
        let thread = thread#push_string lexeme in
        let* state = in_line thread state lexbuf
        in scan state lexbuf
    | Some _ | None ->

      let () = Lexbuf.rollback lexbuf in
      let* state = 
        match
          try Stdlib.Ok (dialect_scan state lexbuf) with
            Error msg -> Stdlib.Error msg
        with
          Ok (token, state) ->
           Ok (state#push_token token)
        | Error message ->
           let used_units = List.rev state#lexical_units
           in Error {used_units; message}
      in
      scan state lexbuf
    }
       
| '#' blank* (directive as id) {
    match id with
      "include" ->
        scan_dir   ~callback:scan Directive.scan_include state lexbuf
    | "import" ->
        scan_dir   ~callback:scan Directive.scan_import  state lexbuf
    | "if" ->
        scan_if    ~callback:scan Directive.scan_if      state lexbuf
    | "elif" ->
        scan_elif  ~callback:scan Directive.scan_elif    state lexbuf
    | "else" ->
        scan_else  ~callback:scan Directive.scan_else    state lexbuf
    | "endif" ->
        scan_endif ~callback:scan Directive.scan_endif   state lexbuf
    | "define" ->
        scan_dir   ~callback:scan Directive.scan_define  state lexbuf
    | "undef" ->
        scan_dir   ~callback:scan Directive.scan_undef   state lexbuf
    | "error" ->
        scan_dir   ~callback:scan Directive.scan_error   state lexbuf
    | _ ->

      let () = Lexbuf.rollback lexbuf in
      let* state = 
        match
          try Stdlib.Ok (dialect_scan state lexbuf) with
            Error msg -> Stdlib.Error msg
        with
          Ok (token, state) ->
           Ok (state#push_token token)
        | Error message ->
           let used_units = List.rev state#lexical_units
           in Error {used_units; message}
      in
      scan state lexbuf
}

  (* Linemarkers preprocessing directives (from #include) *)

| '#' blank* (natural as linenum) {
                 try let* state = scan_linemarker linenum state lexbuf in scan state lexbuf with
                 | DontScan state -> Ok state
                 | ScanOneMoreTime (state, hash_state) ->
              (* The linemarker has been produced by the start of the
                 preprocessing of an #include directive. See case above
                 ([Pop]).

                   We call recursively [callback] to scan until a
                 linemarker with a flag "2" is found, that is, the case
                 [Pop] above. Between a [Push] linemarker and a [Pop],
                 we assume that we scan the contents of the included
                 file. *)
                    let* state = scan state lexbuf in
              (* The contents of the included file was scanned
                 successfully, that is, the case [Pop] above was
                 hit. We restore the position saved at the start of
                 this semantic action, that is, just before the "#" of
                 the [Push] linemarker. With that position committed
                 to the state, we call recursively [callback] to
                 resume scanning the rest of the file corresponding to
                 what was just after the original #include. *)
                    scan (state#set_pos hash_state#pos) lexbuf


               }


  (* End-of-File: we return the final state *)

| eof {
  match
    try Stdlib.Ok (dialect_scan state lexbuf) with
      Error msg -> Stdlib.Error msg
  with
    Ok (token, state) ->
     Ok (state#push_token token)
  | Error message ->
     let used_units = List.rev state#lexical_units
     in Error {used_units; message}
}

  (* Other tokens *)

| _ {

      let () = Lexbuf.rollback lexbuf in
      let* state = 
        match
          try Stdlib.Ok (dialect_scan state lexbuf) with
            Error msg -> Stdlib.Error msg
        with
          Ok (token, state) ->
           Ok (state#push_token token)
        | Error message ->
           let used_units = List.rev state#lexical_units
           in Error {used_units; message}
      in
      scan state lexbuf
}

(* Block comments *)

and in_block block thread state = parse
  string_delimiter {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.string with
      Some delimiter when String.(delimiter = lexeme) ->
        let opening = thread#opening in
        let state, Region.{region; _} = state#sync lexbuf in
        let thread = thread#push_string lexeme in
        let thread = thread#set_opening region in
        let* thread, state = in_string thread state lexbuf in
        let thread = thread#push_string lexeme in
        let thread = thread#set_opening opening
        in in_block block thread state lexbuf
    | Some _ | None ->
        scan_utf8_wrap (scan_utf8 open_block) (in_block block)
                       thread state lexbuf }

| block_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    if   String.(block#opening = lexeme)
    then let opening = thread#opening in
         let state, Region.{region; _} = state#sync lexbuf in
         let thread = thread#push_string lexeme in
         let thread = thread#set_opening region in
         let* thread, state = in_block block thread state lexbuf in
         let thread = thread#set_opening opening
         in in_block block thread state lexbuf
    else scan_utf8_wrap (scan_utf8 open_block) (in_block block)
                        thread state lexbuf }

| block_comment_closing {
    let state, Region.{value=lexeme; _} = state#sync lexbuf in
    if   String.(block#closing = lexeme)
    then Ok (thread#push_string lexeme, state)
    else scan_utf8_wrap (scan_utf8 open_block)
                        (in_block block)
                        thread state lexbuf }
| nl as nl {
    let thread = thread#push_string nl
    and state  = state#newline lexbuf
    in in_block block thread state lexbuf }

| eof { fail state thread#opening LexerLib.Error.Unterminated_comment }

| _ { scan_utf8_wrap (scan_utf8 open_block)
                     (in_block block)
                     thread state lexbuf }

(* Line comments *)

and in_line thread state = parse
  nl as _nl { let state = state#push_line thread in
             Ok (state#push_newline None lexbuf) }
| eof      { Ok (state#push_line thread) }
| _        { let scan_utf8 =
               scan_utf8 (fun thread state -> Ok (thread, state))
             in scan_utf8_wrap scan_utf8 in_line thread state lexbuf }

(* Scanning UTF-8 encoded characters *)

and scan_utf8 if_eof thread state = parse
  eof { if_eof thread state }
| _   { let lexeme = Lexing.lexeme lexbuf in
        let thread = thread#push_string lexeme in
        let     () = state#supply (Bytes.of_string lexeme) 0 1 in
        match Uutf.decode state#decoder with
          `Uchar _     -> Ok (thread, state)
        | `Malformed _
        | `End         -> Error (thread, state,
                                 LexerLib.Error.Invalid_utf8_sequence)
        | `Await       -> scan_utf8 if_eof thread state lexbuf }

(* Scanning strings *)

and in_string thread state = parse
  string_delimiter {
         let state, Region.{value; region} = state#sync lexbuf in
         let lexeme = value in
         match Config.string with
           Some delimiter when String.(delimiter = lexeme) ->
             (* Closing the string *)
             Ok (thread#set_closing region, state)
         | Some _ | None -> (* Still inside the string *)
             let thread = thread#push_string lexeme
             in in_string thread state lexbuf }
| '\\' { let state, Region.{region; _} = state#sync lexbuf
         in unescape region thread state lexbuf }
| nl   { fail state thread#opening LexerLib.Error.Newline_in_string }
| eof  { fail state thread#opening LexerLib.Error.Unterminated_string }
| ['\000' - '\031'] | ['\128' - '\255'] as c
           (* Control characters and 8-bit ASCII *)
       { let _, Region.{region; _} = state#sync lexbuf in
         fail state region (LexerLib.Error.Invalid_character_in_string c) }
| _    { let state, Region.{value; _} = state#sync lexbuf in
         in_string (thread#push_string value) state lexbuf }

and unescape backslash thread state = parse
  string_delimiter {
         let state, Region.{value=lexeme; _} = state#sync lexbuf in
         let interpretation =
           match Config.string with
             Some delimiter when String.(delimiter = lexeme) ->
               lexeme (* E.g. unescaped \" into " *)
           | Some _ | None -> "\\" ^ lexeme (* verbatim *) in
         let thread = thread#push_string interpretation
         in in_string thread state lexbuf }
| 'n'  { let state, _ = state#sync lexbuf
         (* Unescaped "\n" into '\010': *)
         and thread = thread#push_char '\n'
         in in_string thread state lexbuf }
| '\\' { let state, Region.{value=lexeme; _} = state#sync lexbuf in
         (* Unescaped "\\" into '\\': *)
         let thread = thread#push_string lexeme
         in in_string thread state lexbuf }
| _    { let _, Region.{region; _} = state#sync lexbuf in
         let region = Region.cover backslash region in
         fail state region Undefined_escape_sequence }

(* Scanner called first *)

and init state = parse
  utf8_bom { scan (state#push_bom lexbuf) lexbuf       }
| eof {
  match
    try Stdlib.Ok (dialect_scan state lexbuf) with
      Error msg -> Stdlib.Error msg
  with
    Ok (token, state) ->
     Ok (state#push_token token)
  | Error message ->
     let used_units = List.rev state#lexical_units
     in Error {used_units; message}
}
| _        { Lexbuf.rollback lexbuf; scan state lexbuf }

(* END LEXERLIB DEFINITION *)


(* DIALECT RULES *)

(* The scanner [scan] has a parameter [state] that is threaded through
   recursive calls. We start with the special cases so if they fail in
   their semantic actions, the normal cases can be tried next. *)

and dialect_scan state = parse
  "`" | "{|" as lexeme {
    let verb_open, verb_close = Token.verbatim_delimiters in
    if String.(lexeme = verb_open) then
      let state, Region.{region; _} = state#sync lexbuf in
      let thread = Thread.make ~opening:region
      in scan_verbatim verb_close thread state lexbuf
         |> mk_verbatim
    else mk_sym state lexbuf }

| "[@" str_attr "]"  { mk_str_attr key ?value state lexbuf }
| "[@" id_attr  "]"  { mk_id_attr  key ?value state lexbuf }
| ident | ext_ident  { mk_ident               state lexbuf }
| uident             { mk_uident              state lexbuf }
| bytes              { mk_bytes bytes         state lexbuf }
| nat "n"            { mk_nat   nat           state lexbuf }
| nat "mutez"        { mk_mutez nat           state lexbuf }
| nat tz_or_tez      { mk_tez   nat tez       state lexbuf }
| natural            { mk_int                 state lexbuf }
| symbol             { mk_sym                 state lexbuf }
| eof                { mk_eof                 state lexbuf }
| code_inj           { mk_lang  start lang    state lexbuf }
| decimal tz_or_tez  { mk_tez_dec integral fractional
                                          tez state lexbuf }

| _ as c { let _, Region.{region; _} = state#sync lexbuf
           in fail_with_region region (Unexpected_character c) }

(* Attribute scanning for JsLIGO. Accumulator [acc] is list of
   previous tokens in reverse order. *)

and line_comment_attr acc state = parse
  "//" blank* { let state = state#sync lexbuf |> fst in
                scan_attributes true scan_eof acc state lexbuf }
| eof | _     { Lexbuf.rollback lexbuf; acc }

and scan_attributes first_call scan_end acc state = parse
  '@' id_attr {
    let attr, state = mk_id_attr key ?value state lexbuf in
    let state       = scan_blanks state lexbuf in
    scan_attributes false scan_end (attr::acc) state lexbuf }
| '@' str_attr {
    let attr, state = mk_str_attr key ?value state lexbuf in
    let state       = scan_blanks state lexbuf in
    scan_attributes false scan_end (attr::acc) state lexbuf }
| eof | _ {
    Lexbuf.rollback lexbuf;
    if first_call then acc else scan_end acc state lexbuf }

and scan_blanks state = parse
  blank* { state#sync lexbuf |> fst }

and scan_eof acc state = parse
  blank* eof { acc }
| _          { let _, Region.{region; _} = state#sync lexbuf
               in fail_with_region region Unterminated_comment }

and block_comment_attr acc state = parse
  "/*" blank* { let state = state#sync lexbuf |> fst in
                scan_attributes true scan_close acc state lexbuf }
| eof | _ { Lexbuf.rollback lexbuf; scan_close acc state lexbuf }

and scan_close acc state = parse
  blank* "*/" eof { acc }
| eof | _ { let _, Region.{region; _} = state#sync lexbuf
            in fail_with_region region Unterminated_comment }

(* Scanning verbatim strings with or without inclusion of Michelson
   code *)

and scan_verbatim verb_close thread state = parse
  (* Here is a hack to scan and ignore linemarkers in verbatim
     strings. Those could be the result of preprocessing an #include
     directive, or manually inserted. *)

  '#' blank* (natural as linenum) {
    let hash_state    = state in
    let state, _      = state#sync lexbuf in
    let preproc_state = new Preprocessor.State.t state#pos in
    let linenum       = Region.wrap_ghost linenum in (* We don't care. *)
    (match Directive.scan_linemarker
             hash_state#pos linenum preproc_state lexbuf
     with Stdlib.Error (region, error) ->
            fail_with_region region (Invalid_directive error)
        | Ok _ ->
            let state = hash_state#newline lexbuf in
            scan_verbatim verb_close thread state lexbuf) }

| "`" | "|}" as lexeme {
    if String.(verb_close = lexeme) then
      thread, fst (state#sync lexbuf)
    else
      let state, _ = state#sync lexbuf
      and thread   = thread#push_string lexeme in
      scan_verbatim verb_close thread state lexbuf }

| nl  { let nl     = Lexing.lexeme lexbuf in
        let ()     = Lexing.new_line lexbuf
        and state  = state#set_pos (state#pos#new_line nl)
        and thread = thread#push_string nl in
        scan_verbatim verb_close thread state lexbuf }

| eof { fail_with_region thread#opening (Unterminated_verbatim verb_close) }

| _   { let lexeme   = Lexing.lexeme lexbuf in
        let state, _ = state#sync lexbuf
        and thread   = thread#push_string lexeme in
        scan_verbatim verb_close thread state lexbuf }

(* END LEXER DEFINITION *)

{
(* START CORE TRAILER *)

    let open_stream : Lexbuf.input -> (instance, message) result =
      let first_call = ref true in
      let scan state =
        (if !first_call then (first_call := false; init) else scan)
        state
      in open_stream scan

(* START DIALECT TRAILER *)

    let mk_state lexbuf =
      let file  = Lexbuf.current_filename lexbuf
      and line  = Lexbuf.current_linenum lexbuf in
      let state = State.empty ~file in
      let pos   = state#pos#set_line line
      in state#set_pos pos

    type 'src lexer = 'src -> (units, error) result

    let line_comment_attr acc lexbuf =
      let state = mk_state lexbuf in
      try Stdlib.Ok (line_comment_attr acc state lexbuf) with
        Error msg -> Stdlib.Error msg


    let block_comment_attr acc lexbuf =
      let state = mk_state lexbuf in
      try Stdlib.Ok (block_comment_attr acc state lexbuf) with
        Error msg -> Stdlib.Error msg

    (* Lexing the input given a lexer instance *)

    let scan_all_units: (instance, message) result -> (units, error) result  = function
      Stdlib.Error message ->
        Caml.flush_all (); Error {used_units=[]; message}
    | Ok {read_units; lexbuf; close; _} ->
        let result = read_units lexbuf
        in (Caml.flush_all (); close (); result)

    (* Lexing all lexical units from various sources *)

    let from_lexbuf ?(file="") lexbuf =
      (open_stream (Lexbuf (file, lexbuf))) |> scan_all_units

    let from_channel ?(file="") channel =
      (open_stream (Channel (file, channel))) |> scan_all_units

    let from_string ?(file="") string =
      (open_stream (String (file, string))) |> scan_all_units

    let from_buffer ?(file="") buffer =
      (open_stream (Buffer (file, buffer))) |> scan_all_units

    let from_file file = (open_stream (File file)) |> scan_all_units
  end
(* END LowAPI *)

(* END TRAILER *)
}
