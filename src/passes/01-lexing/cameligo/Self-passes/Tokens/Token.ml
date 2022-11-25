(* Token specification for CameLIGO *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Markup    = LexerLib.Markup
module Directive = Preprocessor.Directive

(* Utility modules and types *)

module SMap = Map.Make (String)
module Wrap = Lexing_shared.Wrap
module Attr = Lexing_shared.Attr

let sprintf = Printf.sprintf

let wrap = Wrap.wrap

module T =
  struct
    (* A lexeme is the concrete syntax of a token *)

    type lexeme = string

    (* Definition of tokens generated by "menhir --only-tokens"

       It contains [token] and ['a terminal] types. The first one we
       redefine manually here (by type [t]) but the second one we need
       to satisfy Menhir's Inspection API.  *)

    include Menhir_cameligo_tokens.MenhirToken

    (* TOKENS *)

    type t =
      (* Preprocessing directives *)

      Directive of Directive.t

      (* Literals *)

    | String   of lexeme Wrap.t
    | Verbatim of lexeme Wrap.t
    | Bytes    of (lexeme * Hex.t) Wrap.t
    | Int      of (lexeme * Z.t) Wrap.t
    | Nat      of (lexeme * Z.t) Wrap.t
    | Mutez    of (lexeme * Int64.t) Wrap.t
    | Ident    of lexeme Wrap.t
    | UIdent   of lexeme Wrap.t
    | Lang     of lexeme Region.reg Region.reg
    | Attr     of Attr.t Region.reg

    (* Symbols *)

    | ARROW    of lexeme Wrap.t  (* -> *)
    | CONS     of lexeme Wrap.t  (* :: *)
    | CARET    of lexeme Wrap.t  (* ^  *)
    | MINUS    of lexeme Wrap.t  (* -  *)
    | PLUS     of lexeme Wrap.t  (* +  *)
    | SLASH    of lexeme Wrap.t  (* /  *)
    | TIMES    of lexeme Wrap.t  (* *  *)
    | LPAR     of lexeme Wrap.t  (* (  *)
    | RPAR     of lexeme Wrap.t  (* )  *)
    | LBRACKET of lexeme Wrap.t  (* [  *)
    | RBRACKET of lexeme Wrap.t  (* ]  *)
    | LBRACE   of lexeme Wrap.t  (* {  *)
    | RBRACE   of lexeme Wrap.t  (* }  *)
    | COMMA    of lexeme Wrap.t  (* ,  *)
    | SEMI     of lexeme Wrap.t  (* ;  *)
    | VBAR     of lexeme Wrap.t  (* |  *)
    | COLON    of lexeme Wrap.t  (* :  *)
    | DOT      of lexeme Wrap.t  (* .  *)
    | WILD     of lexeme Wrap.t  (* _  *)
    | EQ       of lexeme Wrap.t  (* =  *)
    | NE       of lexeme Wrap.t  (* <> *)
    | LT       of lexeme Wrap.t  (* <  *)
    | GT       of lexeme Wrap.t  (* >  *)
    | LE       of lexeme Wrap.t  (* <= *)
    | GE       of lexeme Wrap.t  (* >= *)
    | BOOL_OR  of lexeme Wrap.t  (* || *)
    | BOOL_AND of lexeme Wrap.t  (* && *)
    | QUOTE    of lexeme Wrap.t  (* '  *)
    | REV_APP  of lexeme Wrap.t  (* |> *)

    (* Keywords *)

    | Begin     of lexeme Wrap.t  (* begin  *)
    | Else      of lexeme Wrap.t  (* else   *)
    | End       of lexeme Wrap.t  (* end    *)
    | Fun       of lexeme Wrap.t  (* fun    *)
    | If        of lexeme Wrap.t  (* if     *)
    | In        of lexeme Wrap.t  (* in     *)
    | Land      of lexeme Wrap.t  (* land   *)
    | Let       of lexeme Wrap.t  (* let    *)
    | Lor       of lexeme Wrap.t  (* lor    *)
    | Lsl       of lexeme Wrap.t  (* lsl    *)
    | Lsr       of lexeme Wrap.t  (* lsr    *)
    | Lxor      of lexeme Wrap.t  (* lxor   *)
    | Match     of lexeme Wrap.t  (* match  *)
    | Mod       of lexeme Wrap.t  (* mod    *)
    | Module    of lexeme Wrap.t  (* module *)
    | Open      of lexeme Wrap.t  (* open   *)
    | Include   of lexeme Wrap.t  (* include*)
    | Not       of lexeme Wrap.t  (* not    *)
    | Of        of lexeme Wrap.t  (* of     *)
    | Or        of lexeme Wrap.t  (* or     *)
    | Rec       of lexeme Wrap.t  (* rec    *)
    | Struct    of lexeme Wrap.t  (* struct *)
    | Then      of lexeme Wrap.t  (* then   *)
    | Type      of lexeme Wrap.t  (* type   *)
    | With      of lexeme Wrap.t  (* with   *)

    (* End-Of-File *)

    | EOF of lexeme Wrap.t

    type token = t


    (* FROM TOKENS TO LEXEMES *)

    let to_lexeme = function
      (* Directives *)

      Directive d -> (Directive.to_lexeme d).Region.value

      (* Literals *)

    | String t   -> sprintf "%S" t#payload (* Escaped *)
    | Verbatim t -> String.escaped t#payload
    | Bytes t    -> fst t#payload
    | Int t
    | Nat t      -> fst t#payload
    | Mutez t    -> fst t#payload
    | Ident t
    | UIdent t   -> t#payload
    | Attr t     -> Attr.to_lexeme t.Region.value
    | Lang lang  -> "[%" ^ Region.(lang.value.value)

    (* Symbols *)

    | ARROW    t
    | CONS     t
    | CARET    t
    | MINUS    t
    | PLUS     t
    | SLASH    t
    | TIMES    t
    | LPAR     t
    | RPAR     t
    | LBRACKET t
    | RBRACKET t
    | LBRACE   t
    | RBRACE   t
    | COMMA    t
    | SEMI     t
    | VBAR     t
    | COLON    t
    | DOT      t
    | WILD     t
    | EQ       t
    | NE       t
    | LT       t
    | GT       t
    | LE       t
    | GE       t
    | BOOL_OR  t
    | BOOL_AND t
    | QUOTE    t
    | REV_APP  t

    (* Keywords *)

    | Begin  t
    | Else   t
    | End    t
    | Fun    t
    | If     t
    | In     t
    | Land   t
    | Let    t
    | Lor    t
    | Lsl    t
    | Lsr    t
    | Lxor   t
    | Match  t
    | Mod    t
    | Module t
    | Open   t
    | Include t
    | Not    t
    | Of     t
    | Or     t
    | Rec    t
    | Struct t
    | Then   t
    | Type   t
    | With   t -> t#payload

    (* End-Of-File *)

    | EOF _ -> ""


    (* KEYWORDS *)

    let wrap_begin  = wrap "begin"
    let wrap_else   = wrap "else"
    let wrap_end    = wrap "end"
    let wrap_fun    = wrap "fun"
    let wrap_rec    = wrap "rec"
    let wrap_if     = wrap "if"
    let wrap_in     = wrap "in"
    let wrap_let    = wrap "let"
    let wrap_match  = wrap "match"
    let wrap_mod    = wrap "mod"
    let wrap_land   = wrap "land"
    let wrap_lor    = wrap "lor"
    let wrap_lxor   = wrap "lxor"
    let wrap_lsl    = wrap "lsl"
    let wrap_lsr    = wrap "lsr"
    let wrap_module = wrap "module"
    let wrap_open   = wrap "open"
    let wrap_include= wrap "include"
    let wrap_not    = wrap "not"
    let wrap_of     = wrap "of"
    let wrap_or     = wrap "or"
    let wrap_struct = wrap "struct"
    let wrap_then   = wrap "then"
    let wrap_type   = wrap "type"
    let wrap_with   = wrap "with"

    (* Smart constructors *)

    let mk_Begin  region = Begin  (wrap_begin  region)
    let mk_Else   region = Else   (wrap_else   region)
    let mk_End    region = End    (wrap_end    region)
    let mk_Fun    region = Fun    (wrap_fun    region)
    let mk_If     region = If     (wrap_if     region)
    let mk_In     region = In     (wrap_in     region)
    let mk_Land   region = Land   (wrap_land   region)
    let mk_Let    region = Let    (wrap_let    region)
    let mk_Lor    region = Lor    (wrap_lor    region)
    let mk_Lsl    region = Lsl    (wrap_lsl    region)
    let mk_Lsr    region = Lsr    (wrap_lsr    region)
    let mk_Lxor   region = Lxor   (wrap_lxor   region)
    let mk_Match  region = Match  (wrap_match  region)
    let mk_Mod    region = Mod    (wrap_mod    region)
    let mk_Module region = Module (wrap_module region)
    let mk_Open   region = Open   (wrap_open   region)
    let mk_Include region = Include (wrap_include region)
    let mk_Not    region = Not    (wrap_not    region)
    let mk_Of     region = Of     (wrap_of     region)
    let mk_Or     region = Or     (wrap_or     region)
    let mk_Rec    region = Rec    (wrap_rec    region)
    let mk_Struct region = Struct (wrap_struct region)
    let mk_Then   region = Then   (wrap_then   region)
    let mk_Type   region = Type   (wrap_type   region)
    let mk_With   region = With   (wrap_with   region)

    (* All keyword smart constructors *)

    let keywords = [
      mk_Begin;
      mk_Else;
      mk_End;
      mk_Fun;
      mk_If;
      mk_In;
      mk_Land;
      mk_Let;
      mk_Lor;
      mk_Lsl;
      mk_Lsr;
      mk_Lxor;
      mk_Match;
      mk_Mod;
      mk_Module;
      mk_Open;
      mk_Include;
      mk_Not;
      mk_Of;
      mk_Or;
      mk_Rec;
      mk_Struct;
      mk_Then;
      mk_Type;
      mk_With
    ]

    (* All keywords *)

    let keywords =
      let add map (key, data) =
        match SMap.add ~key ~data map with
          `Ok map -> map
        | `Duplicate -> map in
      let apply map mk_kwd =
        add map (to_lexeme (mk_kwd Region.ghost), mk_kwd)
      in List.fold_left ~f:apply ~init:SMap.empty keywords

    (* Ghost keywords *)

    let ghost_begin  = wrap_begin  Region.ghost
    let ghost_else   = wrap_else   Region.ghost
    let ghost_end    = wrap_end    Region.ghost
    let ghost_fun    = wrap_fun    Region.ghost
    let ghost_if     = wrap_if     Region.ghost
    let ghost_in     = wrap_in     Region.ghost
    let ghost_land   = wrap_land   Region.ghost
    let ghost_let    = wrap_let    Region.ghost
    let ghost_lor    = wrap_lor    Region.ghost
    let ghost_lsl    = wrap_lsl    Region.ghost
    let ghost_lsr    = wrap_lsr    Region.ghost
    let ghost_lxor   = wrap_lxor   Region.ghost
    let ghost_match  = wrap_match  Region.ghost
    let ghost_mod    = wrap_mod    Region.ghost
    let ghost_module = wrap_module Region.ghost
    let ghost_open   = wrap_open   Region.ghost
    let ghost_include= wrap_include Region.ghost
    let ghost_not    = wrap_not    Region.ghost
    let ghost_of     = wrap_of     Region.ghost
    let ghost_or     = wrap_or     Region.ghost
    let ghost_rec    = wrap_rec    Region.ghost
    let ghost_struct = wrap_struct Region.ghost
    let ghost_then   = wrap_then   Region.ghost
    let ghost_type   = wrap_type   Region.ghost
    let ghost_with   = wrap_with   Region.ghost

    let ghost_Begin  = Begin  ghost_begin
    let ghost_Else   = Else   ghost_else
    let ghost_End    = End    ghost_end
    let ghost_Fun    = Fun    ghost_fun
    let ghost_If     = If     ghost_if
    let ghost_In     = In     ghost_in
    let ghost_Land   = Land   ghost_land
    let ghost_Let    = Let    ghost_let
    let ghost_Lor    = Lor    ghost_lor
    let ghost_Lsl    = Lsl    ghost_lsl
    let ghost_Lsr    = Lsr    ghost_lsr
    let ghost_Lxor   = Lxor   ghost_lxor
    let ghost_Match  = Match  ghost_match
    let ghost_Mod    = Mod    ghost_mod
    let ghost_Module = Module ghost_module
    let ghost_Open   = Open   ghost_open
    let ghost_Include = Include ghost_include
    let ghost_Not    = Not    ghost_not
    let ghost_Of     = Of     ghost_of
    let ghost_Or     = Or     ghost_or
    let ghost_Rec    = Rec    ghost_rec
    let ghost_Struct = Struct ghost_struct
    let ghost_Then   = Then   ghost_then
    let ghost_Type   = Type   ghost_type
    let ghost_With   = With   ghost_with


    (* SYMBOLS *)

    let wrap_arrow    = wrap "->"
    let wrap_cons     = wrap "::"
    let wrap_caret    = wrap "^"
    let wrap_minus    = wrap "-"
    let wrap_plus     = wrap "+"
    let wrap_slash    = wrap "/"
    let wrap_times    = wrap "*"
    let wrap_lpar     = wrap "("
    let wrap_rpar     = wrap ")"
    let wrap_lbracket = wrap "["
    let wrap_rbracket = wrap "]"
    let wrap_lbrace   = wrap "{"
    let wrap_rbrace   = wrap "}"
    let wrap_comma    = wrap ","
    let wrap_semi     = wrap ";"
    let wrap_vbar     = wrap "|"
    let wrap_colon    = wrap ":"
    let wrap_dot      = wrap "."
    let wrap_wild     = wrap "_"
    let wrap_eq       = wrap "="
    let wrap_ne       = wrap "<>"
    let wrap_lt       = wrap "<"
    let wrap_gt       = wrap ">"
    let wrap_le       = wrap "<="
    let wrap_ge       = wrap ">="
    let wrap_bool_or  = wrap "||"
    let wrap_bool_and = wrap "&&"
    let wrap_quote    = wrap "'"
    let wrap_rev_app  = wrap "|>"

    (* Smart constructors *)

    let mk_ARROW    region = ARROW    (wrap_arrow    region)
    let mk_CONS     region = CONS     (wrap_cons     region)
    let mk_CARET    region = CARET    (wrap_caret    region)
    let mk_MINUS    region = MINUS    (wrap_minus    region)
    let mk_PLUS     region = PLUS     (wrap_plus     region)
    let mk_SLASH    region = SLASH    (wrap_slash    region)
    let mk_TIMES    region = TIMES    (wrap_times    region)
    let mk_LPAR     region = LPAR     (wrap_lpar     region)
    let mk_RPAR     region = RPAR     (wrap_rpar     region)
    let mk_LBRACKET region = LBRACKET (wrap_lbracket region)
    let mk_RBRACKET region = RBRACKET (wrap_rbracket region)
    let mk_LBRACE   region = LBRACE   (wrap_lbrace   region)
    let mk_RBRACE   region = RBRACE   (wrap_rbrace   region)
    let mk_COMMA    region = COMMA    (wrap_comma    region)
    let mk_SEMI     region = SEMI     (wrap_semi     region)
    let mk_VBAR     region = VBAR     (wrap_vbar     region)
    let mk_COLON    region = COLON    (wrap_colon    region)
    let mk_DOT      region = DOT      (wrap_dot      region)
    let mk_WILD     region = WILD     (wrap_wild     region)
    let mk_EQ       region = EQ       (wrap_eq       region)
    let mk_NE       region = NE       (wrap_ne       region)
    let mk_LT       region = LT       (wrap_lt       region)
    let mk_GT       region = GT       (wrap_gt       region)
    let mk_LE       region = LE       (wrap_le       region)
    let mk_GE       region = GE       (wrap_ge       region)
    let mk_BOOL_OR  region = BOOL_OR  (wrap_bool_or  region)
    let mk_BOOL_AND region = BOOL_AND (wrap_bool_and region)
    let mk_QUOTE    region = QUOTE    (wrap_quote    region)
    let mk_REV_APP  region = REV_APP  (wrap_rev_app  region)

    (* All symbol smart constructors *)

    let symbols = [
      mk_ARROW;
      mk_CONS;
      mk_CARET;
      mk_MINUS;
      mk_PLUS;
      mk_SLASH;
      mk_TIMES;
      mk_LPAR;
      mk_RPAR;
      mk_LBRACKET;
      mk_RBRACKET;
      mk_LBRACE;
      mk_RBRACE;
      mk_COMMA;
      mk_SEMI;
      mk_VBAR;
      mk_COLON;
      mk_DOT;
      mk_WILD;
      mk_EQ;
      mk_NE;
      mk_LT;
      mk_GT;
      mk_LE;
      mk_GE;
      mk_BOOL_OR;
      mk_BOOL_AND;
      mk_QUOTE;
      mk_REV_APP
    ]

    (* All symbols *)

    let symbols =
      let add map (key, data) =
        match SMap.add ~key ~data map with
          `Ok map -> map
        | `Duplicate -> map in
      let apply map mk_kwd =
        add map (to_lexeme (mk_kwd Region.ghost), mk_kwd)
      in List.fold_left ~f:apply ~init:SMap.empty symbols

    (* Ghost symbols *)

    let ghost_arrow    = wrap_arrow    Region.ghost
    let ghost_cons     = wrap_cons     Region.ghost
    let ghost_caret    = wrap_caret    Region.ghost
    let ghost_minus    = wrap_caret    Region.ghost
    let ghost_plus     = wrap_plus     Region.ghost
    let ghost_slash    = wrap_slash    Region.ghost
    let ghost_times    = wrap_times    Region.ghost
    let ghost_lpar     = wrap_lpar     Region.ghost
    let ghost_rpar     = wrap_rpar     Region.ghost
    let ghost_lbracket = wrap_lbracket Region.ghost
    let ghost_rbracket = wrap_rbracket Region.ghost
    let ghost_lbrace   = wrap_lbrace   Region.ghost
    let ghost_rbrace   = wrap_rbrace   Region.ghost
    let ghost_comma    = wrap_comma    Region.ghost
    let ghost_semi     = wrap_semi     Region.ghost
    let ghost_vbar     = wrap_vbar     Region.ghost
    let ghost_colon    = wrap_colon    Region.ghost
    let ghost_dot      = wrap_dot      Region.ghost
    let ghost_wild     = wrap_wild     Region.ghost
    let ghost_eq       = wrap_eq       Region.ghost
    let ghost_ne       = wrap_ne       Region.ghost
    let ghost_lt       = wrap_lt       Region.ghost
    let ghost_gt       = wrap_gt       Region.ghost
    let ghost_le       = wrap_le       Region.ghost
    let ghost_ge       = wrap_ge       Region.ghost
    let ghost_bool_or  = wrap_bool_or  Region.ghost
    let ghost_bool_and = wrap_bool_and Region.ghost
    let ghost_quote    = wrap_quote    Region.ghost
    let ghost_rev_app  = wrap_rev_app  Region.ghost

    let ghost_ARROW    = ARROW    ghost_semi
    let ghost_CONS     = CONS     ghost_cons
    let ghost_CARET    = CARET    ghost_caret
    let ghost_MINUS    = MINUS    ghost_minus
    let ghost_PLUS     = PLUS     ghost_plus
    let ghost_SLASH    = SLASH    ghost_slash
    let ghost_TIMES    = TIMES    ghost_times
    let ghost_LPAR     = LPAR     ghost_lpar
    let ghost_RPAR     = RPAR     ghost_rpar
    let ghost_LBRACKET = LBRACKET ghost_lbracket
    let ghost_RBRACKET = RBRACKET ghost_rbracket
    let ghost_LBRACE   = LBRACE   ghost_lbrace
    let ghost_RBRACE   = RBRACE   ghost_rbrace
    let ghost_COMMA    = COMMA    ghost_comma
    let ghost_SEMI     = SEMI     ghost_semi
    let ghost_VBAR     = VBAR     ghost_vbar
    let ghost_COLON    = COLON    ghost_colon
    let ghost_DOT      = DOT      ghost_dot
    let ghost_WILD     = WILD     ghost_wild
    let ghost_EQ       = EQ       ghost_eq
    let ghost_NE       = NE       ghost_ne
    let ghost_LT       = LT       ghost_lt
    let ghost_GT       = GT       ghost_gt
    let ghost_LE       = LE       ghost_le
    let ghost_GE       = GE       ghost_ge
    let ghost_BOOL_OR  = BOOL_OR  ghost_bool_or
    let ghost_BOOL_AND = BOOL_AND ghost_bool_and
    let ghost_QUOTE    = QUOTE    ghost_quote
    let ghost_REV_APP  = REV_APP  ghost_rev_app


    (* OTHER GHOST TOKENS *)

    (* IMPORTANT: These values cannot be exported in Token.mli *)

    let wrap_string   s = Wrap.wrap s
    let wrap_verbatim s = Wrap.wrap s
    let wrap_bytes    b = Wrap.wrap ("0x" ^ Hex.show b, b)
    let wrap_int      z = Wrap.wrap (Z.to_string z, z)
    let wrap_nat      z = Wrap.wrap (Z.to_string z ^ "n", z)
    let wrap_mutez    i = Wrap.wrap (Int64.to_string i ^ "mutez", i)
    let wrap_ident    i = Wrap.wrap i
    let wrap_uident   c = Wrap.wrap c

    let wrap_attr key value region =
      Region.{value = (key, value); region}

    let wrap_lang lang region =
      let start = region#start#shift_bytes (String.length "[%") in
      let lang_reg = Region.make ~start ~stop:region#stop in
      Region.{region; value = {value=lang; region=lang_reg}}

    let ghost_string   s = wrap_string   s   Region.ghost
    let ghost_verbatim s = wrap_verbatim s   Region.ghost
    let ghost_bytes    b = wrap_bytes    b   Region.ghost
    let ghost_int      z = wrap_int      z   Region.ghost
    let ghost_nat      z = wrap_nat      z   Region.ghost
    let ghost_mutez    i = wrap_mutez    i   Region.ghost
    let ghost_ident    i = wrap_ident    i   Region.ghost
    let ghost_uident   c = wrap_uident   c   Region.ghost
    let ghost_attr   k v = wrap_attr     k v Region.ghost
    let ghost_lang     l = wrap_lang     l   Region.ghost

    let ghost_String   s = String   (ghost_string s)
    let ghost_Verbatim s = Verbatim (ghost_verbatim s)
    let ghost_Bytes    b = Bytes    (ghost_bytes b)
    let ghost_Int      z = Int      (ghost_int z)
    let ghost_Nat      z = Nat      (ghost_nat z)
    let ghost_Mutez    i = Mutez    (ghost_mutez i)
    let ghost_Ident    i = Ident    (ghost_ident i)
    let ghost_UIdent   c = UIdent   (ghost_uident c)
    let ghost_Attr   k v = Attr     (ghost_attr k v)
    let ghost_Lang     l = Lang     (ghost_lang l)

    (* END-OF-FILE TOKEN *)

    let wrap_eof      = wrap ""
    let mk_EOF region = EOF (wrap_eof region)
    let ghost_eof     = wrap_eof Region.ghost
    let ghost_EOF     = mk_EOF Region.ghost


    (* FROM TOKEN STRINGS TO LEXEMES *)

    let concrete = function
      "Ident"    -> "x"
    | "UIdent"   -> "C"
    | "Int"      -> "1"
    | "Nat"      -> "1n"
    | "Mutez"    -> "1mutez"
    | "String"   -> "\"a string\""
    | "Verbatim" -> "{|verbatim|}"
    | "Bytes"    -> "0xAA"
    | "Attr"     -> "[@attr]"
    | "Lang"     -> "[%Michelson"

    (* Symbols *)

    | "ARROW"    -> ghost_arrow#payload
    | "CONS"     -> ghost_cons#payload
    | "CARET"    -> ghost_caret#payload
    | "MINUS"    -> ghost_minus#payload
    | "PLUS"     -> ghost_plus#payload
    | "SLASH"    -> ghost_slash#payload
    | "TIMES"    -> ghost_times#payload
    | "LPAR"     -> ghost_lpar#payload
    | "RPAR"     -> ghost_rpar#payload
    | "LBRACE"   -> ghost_lbrace#payload
    | "RBRACE"   -> ghost_rbrace#payload
    | "LBRACKET" -> ghost_lbracket#payload
    | "RBRACKET" -> ghost_rbracket#payload
    | "COMMA"    -> ghost_comma#payload
    | "SEMI"     -> ghost_semi#payload
    | "VBAR"     -> ghost_vbar#payload
    | "COLON"    -> ghost_colon#payload
    | "DOT"      -> ghost_dot#payload
    | "WILD"     -> ghost_wild#payload
    | "EQ"       -> ghost_eq#payload
    | "NE"       -> ghost_ne#payload
    | "LT"       -> ghost_lt#payload
    | "GT"       -> ghost_gt#payload
    | "LE"       -> ghost_le#payload
    | "GE"       -> ghost_ge#payload
    | "BOOL_OR"  -> ghost_bool_or#payload
    | "BOOL_AND" -> ghost_bool_and#payload
    | "QUOTE"    -> ghost_quote#payload
    | "REV_APP"  -> ghost_rev_app#payload

    (* Keywords *)

    | "Begin"  -> ghost_begin#payload
    | "Else"   -> ghost_else#payload
    | "End"    -> ghost_end#payload
    | "Fun"    -> ghost_fun#payload
    | "If"     -> ghost_if#payload
    | "In"     -> ghost_in#payload
    | "Land"   -> ghost_land#payload
    | "Let"    -> ghost_let#payload
    | "Lor"    -> ghost_lor#payload
    | "Lsl"    -> ghost_lsl#payload
    | "Lsr"    -> ghost_lsr#payload
    | "Lxor"   -> ghost_lxor#payload
    | "Match"  -> ghost_match#payload
    | "Mod"    -> ghost_mod#payload
    | "Module" -> ghost_module#payload
    | "Open"   -> ghost_open#payload
    | "Include"-> ghost_include#payload
    | "Not"    -> ghost_not#payload
    | "Of"     -> ghost_of#payload
    | "Or"     -> ghost_or#payload
    | "Rec"    -> ghost_rec#payload
    | "Struct" -> ghost_struct#payload
    | "Then"   -> ghost_then#payload
    | "Type"   -> ghost_type#payload
    | "With"   -> ghost_with#payload

    (* End-Of-File *)

    | "EOF" -> ghost_eof#payload

    (* This case should not happen! *)

    | _  -> "\\Unknown" (* Backslash meant to trigger an error *)


    (* FROM TOKENS TO TOKEN STRINGS AND REGIONS *)

    let proj_token = function
      (* Preprocessing directives *)

      Directive d -> Directive.project d

      (* Literals *)

    | String t ->
        t#region, sprintf "String %S" t#payload
    | Verbatim t ->
        t#region, sprintf "Verbatim %S" t#payload
    | Bytes t ->
        let s, b = t#payload in
        t#region,
        sprintf "Bytes (%S, \"0x%s\")" s (Hex.show b)
    | Int t ->
        let s, n = t#payload in
        t#region, sprintf "Int (%S, %s)" s (Z.to_string n)
    | Nat t ->
        let s, n = t#payload in
        t#region, sprintf "Nat (%S, %s)" s (Z.to_string n)
    | Mutez t ->
        let s, n = t#payload in
        t#region, sprintf "Mutez (%S, %s)" s (Int64.to_string n)
    | Ident t ->
        t#region, sprintf "Ident %S" t#payload
    | UIdent t ->
        t#region, sprintf "UIdent %S" t#payload
    | Attr {region; value} ->
        region, sprintf "Attr %s" (Attr.to_string value)
    | Lang {value = {value = payload; _}; region; _} ->
        region, sprintf "Lang %S" payload

    (* Symbols *)

    | ARROW    t -> t#region, "ARROW"
    | CONS     t -> t#region, "CONS"
    | CARET    t -> t#region, "CARET"
    | MINUS    t -> t#region, "MINUS"
    | PLUS     t -> t#region, "PLUS"
    | SLASH    t -> t#region, "SLASH"
    | TIMES    t -> t#region, "TIMES"
    | LPAR     t -> t#region, "LPAR"
    | RPAR     t -> t#region, "RPAR"
    | LBRACKET t -> t#region, "LBRACKET"
    | RBRACKET t -> t#region, "RBRACKET"
    | LBRACE   t -> t#region, "LBRACE"
    | RBRACE   t -> t#region, "RBRACE"
    | COMMA    t -> t#region, "COMMA"
    | SEMI     t -> t#region, "SEMI"
    | VBAR     t -> t#region, "VBAR"
    | COLON    t -> t#region, "COLON"
    | DOT      t -> t#region, "DOT"
    | WILD     t -> t#region, "WILD"
    | EQ       t -> t#region, "EQ"
    | NE       t -> t#region, "NE"
    | LT       t -> t#region, "LT"
    | GT       t -> t#region, "GT"
    | LE       t -> t#region, "LE"
    | GE       t -> t#region, "GE"
    | BOOL_OR  t -> t#region, "BOOL_OR"
    | BOOL_AND t -> t#region, "BOOL_AND"
    | QUOTE    t -> t#region, "QUOTE"
    | REV_APP  t -> t#region, "REV_APP"

    (* Keywords *)

    | Begin  t -> t#region, "Begin"
    | Else   t -> t#region, "Else"
    | End    t -> t#region, "End"
    | Fun    t -> t#region, "Fun"
    | If     t -> t#region, "If"
    | In     t -> t#region, "In"
    | Land   t -> t#region, "Land"
    | Let    t -> t#region, "Let"
    | Lor    t -> t#region, "Lor"
    | Lsl    t -> t#region, "Lsl"
    | Lsr    t -> t#region, "Lsr"
    | Lxor   t -> t#region, "Lxor"
    | Match  t -> t#region, "Match"
    | Mod    t -> t#region, "Mod"
    | Module t -> t#region, "Module"
    | Open   t -> t#region, "Open"
    | Include t -> t#region, "Include"
    | Not    t -> t#region, "Not"
    | Of     t -> t#region, "Of"
    | Or     t -> t#region, "Or"
    | Rec    t -> t#region, "Rec"
    | Struct t -> t#region, "Struct"
    | Then   t -> t#region, "Then"
    | Type   t -> t#region, "Type"
    | With   t -> t#region, "With"

    (* End-Of-File *)

    | EOF t -> t#region, "EOF"


    (* CONVERSIONS *)

    let to_string ~offsets mode token =
      let region, val_str = proj_token token in
      let reg_str = region#compact ~offsets mode
      in sprintf "%s: %s" reg_str val_str

    let to_region token = proj_token token |> fst


    (* SMART CONSTRUCTORS *)

    (* Keywords *)

    type kwd_err = Invalid_keyword

    let mk_kwd ident region =
      match SMap.find keywords ident with
        Some mk_kwd -> Ok (mk_kwd region)
      |        None -> Error Invalid_keyword

    (* Directives *)

    let mk_directive dir = Directive dir

    (* Strings *)

    let mk_string lexeme region = String (wrap lexeme region)

    (* Verbatim strings *)

    let mk_verbatim lexeme region = Verbatim (wrap lexeme region)

    (* Bytes *)

    let mk_bytes lexeme bytes region =
      Bytes (wrap ("0x" ^ lexeme, `Hex bytes) region)

    (* Integers *)

    let mk_int lexeme z region = Int (wrap (lexeme, z) region)

    (* Natural numbers *)

    type nat_err = Wrong_nat_syntax of string (* Not CameLIGO *)

    let mk_nat nat z region = Ok (Nat (wrap (nat ^ "n", z) region))

    (* Mutez *)

    type mutez_err = Wrong_mutez_syntax of string (* Not CameLIGO *)

    let mk_mutez nat ~suffix int64 region =
      Ok (Mutez (wrap (nat ^ suffix, int64) region))

    (* End-Of-File *)

    let mk_eof region = EOF (wrap "" region)

    (* Symbols *)

    type sym_err = Invalid_symbol of string

    let mk_sym lexeme region =
      match SMap.find symbols lexeme with
        Some mk_sym -> Ok (mk_sym region)
      |        None -> Error (Invalid_symbol lexeme)

    (* Identifiers *)

    let mk_ident value region =
      match SMap.find keywords value with
        Some mk_kwd -> mk_kwd region
      |        None -> Ident (wrap value region)

    (* Constructors/Modules *)

    let mk_uident value region = UIdent (wrap value region)

    (* Attributes *)

    let mk_attr ~key ?value region = Attr {region; value = key, value}

    (* Code injection *)

    type lang_err = Wrong_lang_syntax of string (* Not CameLIGO *)

    let mk_lang lang region = Ok (Lang Region.{value=lang; region})

    (* PREDICATES *)

    let is_int    = function Int    _ -> true | _ -> false
    let is_string = function String _ -> true | _ -> false
    let is_bytes  = function Bytes  _ -> true | _ -> false
    let is_eof    = function EOF    _ -> true | _ -> false

    let hex_digits = ["A"; "B"; "C"; "D"; "E"; "F";
                      "a"; "b"; "c"; "d"; "e"; "f"]

    let is_hex = function
      UIdent t | Ident t ->
        List.mem hex_digits t#payload ~equal:String.equal
    | _ -> false

    let is_sym = function
      ARROW _
    | CONS _
    | CARET _
    | MINUS _
    | PLUS _
    | SLASH _
    | TIMES _
    | LPAR _
    | RPAR _
    | LBRACKET _
    | RBRACKET _
    | LBRACE _
    | RBRACE _
    | COMMA _
    | SEMI _
    | VBAR _
    | COLON _
    | DOT _
    | WILD _
    | REV_APP _
    | EQ _
    | NE _
    | LT _
    | GT _
    | LE _
    | GE _
    | BOOL_OR _
    | BOOL_AND _ -> true
    | _ -> false

    (* Verbatim strings *)

    let verbatim_delimiters = ("{|", "|}")
  end

include T

module type S = module type of T
