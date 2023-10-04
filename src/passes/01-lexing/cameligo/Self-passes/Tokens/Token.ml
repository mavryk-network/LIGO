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

    include Mh_ml_tokens.MenhirToken

    (* TOKENS *)

    type t =
      (* Preprocessing directives *)

      Directive of Directive.t

      (* Comments *)

    | BlockCom of lexeme Wrap.t
    | LineCom  of lexeme Wrap.t

      (* Literals *)

    | String   of lexeme Wrap.t
    | Verbatim of lexeme Wrap.t
    | Bytes    of (lexeme * Hex.t) Wrap.t
    | Int      of (lexeme * Z.t) Wrap.t
    | Nat      of (lexeme * Z.t) Wrap.t
    | Mutez    of (lexeme * Int64.t) Wrap.t
    | Ident    of lexeme Wrap.t              (* foo  *)
    | UIdent   of lexeme Wrap.t              (* Foo  *)
    | EIdent   of lexeme Wrap.t              (* @foo *)
    | Lang     of lexeme Region.reg Wrap.t
    | Attr     of Attr.t Wrap.t

    (* Symbols *)

    | ARROW    of lexeme Wrap.t  (* -> *)
    | ASS      of lexeme Wrap.t  (* := *)
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
    | BOOL_OR  of lexeme Wrap.t  (* || *)
    | BOOL_AND of lexeme Wrap.t  (* && *)
    | QUOTE    of lexeme Wrap.t  (* '  *)
    | REV_APP  of lexeme Wrap.t  (* |> *)
    | PLUS_EQ  of lexeme Wrap.t  (* += *)
    | MINUS_EQ of lexeme Wrap.t  (* -= *)
    | TIMES_EQ of lexeme Wrap.t  (* *= *)
    | SLASH_EQ of lexeme Wrap.t  (* /= *)
    | VBAR_EQ  of lexeme Wrap.t  (* |= *)

    (* OCaml keywords *)

    | Begin       of lexeme Wrap.t  (* begin   *)
    | Do          of lexeme Wrap.t  (* do      *)
    | Done        of lexeme Wrap.t  (* done    *)
    | Downto      of lexeme Wrap.t  (* downto  *)
    | Else        of lexeme Wrap.t  (* else    *)
    | End         of lexeme Wrap.t  (* end     *)
    | False       of lexeme Wrap.t  (* false   *)
    | For         of lexeme Wrap.t  (* for     *)
    | Fun         of lexeme Wrap.t  (* fun     *)
    | If          of lexeme Wrap.t  (* if      *)
    | In          of lexeme Wrap.t  (* in      *)
    | Include     of lexeme Wrap.t  (* include *)
    | Land        of lexeme Wrap.t  (* land    *)
    | Let         of lexeme Wrap.t  (* let     *)
    | Lor         of lexeme Wrap.t  (* lor     *)
    | Lsl         of lexeme Wrap.t  (* lsl     *)
    | Lsr         of lexeme Wrap.t  (* lsr     *)
    | Lxor        of lexeme Wrap.t  (* lxor    *)
    | Match       of lexeme Wrap.t  (* match   *)
    | Mod         of lexeme Wrap.t  (* mod     *)
    | Module      of lexeme Wrap.t  (* module  *)
    | Mut         of lexeme Wrap.t  (* mut     *)
    | Not         of lexeme Wrap.t  (* not     *)
    | Of          of lexeme Wrap.t  (* of      *)
    | Or          of lexeme Wrap.t  (* or      *)
    | Rec         of lexeme Wrap.t  (* rec     *)
    | Sig         of lexeme Wrap.t  (* sig     *)
    | Struct      of lexeme Wrap.t  (* struct  *)
    | Then        of lexeme Wrap.t  (* then    *)
    | True        of lexeme Wrap.t  (* true    *)
    | Type        of lexeme Wrap.t  (* type    *)
    | Val         of lexeme Wrap.t  (* val     *)
    | While       of lexeme Wrap.t  (* while   *)
    | With        of lexeme Wrap.t  (* with    *)

    (* CameLIGO-specific keywords *)

    | ContractOf  of lexeme Wrap.t  (* contract_of  *)
    | ParameterOf of lexeme Wrap.t  (* parameter_of *)
    | Upto        of lexeme Wrap.t  (* upto         *)

    (* Virtual tokens *)

    | ZWSP of lexeme Wrap.t  (* Zero-Width SPace *)

    (* End-Of-File *)

    | EOF of lexeme Wrap.t

    type token = t

    (* FROM TOKENS TO LEXEMES *)

    let to_lexeme = function
      (* Directives *)

      Directive d -> [(Directive.to_lexeme d).Region.value]

      (* Comments *)

    | LineCom  t -> [sprintf "// %s" t#payload]
    | BlockCom t -> [sprintf "(* %s *)" t#payload]

      (* Literals *)

    | String   t -> [sprintf "%S" t#payload] (* Escaped *)
    | Verbatim t -> [String.escaped t#payload]
    | Bytes    t -> [fst t#payload]
    | Int      t
    | Nat      t -> [fst t#payload]
    | Mutez    t -> [fst t#payload]
    | Ident    t
    | UIdent   t
    | EIdent   t -> [t#payload]
    | Attr     t -> [Attr.to_lexeme t#payload]
    | Lang  lang -> ["[%" ^ lang#payload.value]


    (* Symbols *)

    | ARROW    t
    | ASS      t
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
    | BOOL_OR  t
    | BOOL_AND t
    | QUOTE    t
    | REV_APP  t
    | PLUS_EQ  t
    | MINUS_EQ t
    | TIMES_EQ t
    | SLASH_EQ t
    | VBAR_EQ  t

    (* OCaml keywords *)

    | Begin   t
    | Do      t
    | Done    t
    | Downto  t
    | Else    t
    | End     t
    | False   t
    | For     t
    | Fun     t
    | If      t
    | In      t
    | Land    t
    | Let     t
    | Lor     t
    | Lsl     t
    | Lsr     t
    | Lxor    t
    | Match   t
    | Mod     t
    | Include t
    | Module  t
    | Sig     t
    | Val     t
    | Mut     t
    | Not     t
    | Of      t
    | Or      t
    | Rec     t
    | Struct  t
    | Then    t
    | True    t
    | Type    t
    | While   t
    | With    t -> [t#payload]

    (* CameLIGO-specific keywords *)

    | ContractOf  t
    | ParameterOf t
    | Upto        t -> [t#payload]

    (* Virtual tokens *)

    | ZWSP _ -> [""]

    (* End-Of-File *)

    | EOF _ -> [""]


    (* KEYWORDS *)

    (* OCaml keywords *)

    let wrap_begin     = wrap "begin"
    let wrap_do        = wrap "do"
    let wrap_done      = wrap "done"
    let wrap_downto    = wrap "downto"
    let wrap_else      = wrap "else"
    let wrap_end       = wrap "end"
    let wrap_false     = wrap "false"
    let wrap_for       = wrap "for"
    let wrap_fun       = wrap "fun"
    let wrap_if        = wrap "if"
    let wrap_in        = wrap "in"
    let wrap_include   = wrap "include"
    let wrap_land      = wrap "land"
    let wrap_let       = wrap "let"
    let wrap_lor       = wrap "lor"
    let wrap_lsl       = wrap "lsl"
    let wrap_lsr       = wrap "lsr"
    let wrap_lxor      = wrap "lxor"
    let wrap_match     = wrap "match"
    let wrap_mod       = wrap "mod"
    let wrap_module    = wrap "module"
    let wrap_mut       = wrap "mut"
    let wrap_not       = wrap "not"
    let wrap_of        = wrap "of"
    let wrap_or        = wrap "or"
    let wrap_rec       = wrap "rec"
    let wrap_sig       = wrap "sig"
    let wrap_struct    = wrap "struct"
    let wrap_then      = wrap "then"
    let wrap_true      = wrap "true"
    let wrap_type      = wrap "type"
    let wrap_val       = wrap "val"
    let wrap_while     = wrap "while"
    let wrap_with      = wrap "with"

    let mk_Begin   region = Begin     (wrap_begin     region)
    let mk_Do      region = Do        (wrap_do        region)
    let mk_Done    region = Done      (wrap_done      region)
    let mk_Downto  region = Downto    (wrap_downto    region)
    let mk_Else    region = Else      (wrap_else      region)
    let mk_End     region = End       (wrap_end       region)
    let mk_False   region = False     (wrap_false     region)
    let mk_For     region = For       (wrap_for       region)
    let mk_Fun     region = Fun       (wrap_fun       region)
    let mk_If      region = If        (wrap_if        region)
    let mk_In      region = In        (wrap_in        region)
    let mk_Include region = Include   (wrap_include   region)
    let mk_Land    region = Land      (wrap_land      region)
    let mk_Let     region = Let       (wrap_let       region)
    let mk_Lor     region = Lor       (wrap_lor       region)
    let mk_Lsl     region = Lsl       (wrap_lsl       region)
    let mk_Lsr     region = Lsr       (wrap_lsr       region)
    let mk_Lxor    region = Lxor      (wrap_lxor      region)
    let mk_Match   region = Match     (wrap_match     region)
    let mk_Mod     region = Mod       (wrap_mod       region)
    let mk_Module  region = Module    (wrap_module    region)
    let mk_Mut     region = Mut       (wrap_mut       region)
    let mk_Not     region = Not       (wrap_not       region)
    let mk_Of      region = Of        (wrap_of        region)
    let mk_Or      region = Or        (wrap_or        region)
    let mk_Rec     region = Rec       (wrap_rec       region)
    let mk_Sig     region = Sig       (wrap_sig       region)
    let mk_True    region = True      (wrap_true      region)
    let mk_Struct  region = Struct    (wrap_struct    region)
    let mk_Then    region = Then      (wrap_then      region)
    let mk_Type    region = Type      (wrap_type      region)
    let mk_Val     region = Val       (wrap_val       region)
    let mk_While   region = While     (wrap_while     region)
    let mk_With    region = With      (wrap_with      region)

    (* CameLIGO-specific keywords *)

    let wrap_contract_of  = wrap "contract_of"
    let wrap_parameter_of = wrap "parameter_of"
    let wrap_upto         = wrap "upto"

    let mk_ContractOf  region = ContractOf  (wrap_contract_of  region)
    let mk_ParameterOf region = ParameterOf (wrap_parameter_of region)
    let mk_Upto        region = Upto        (wrap_upto         region)

    (* All keyword smart constructors *)

    let keywords = [
      mk_Begin;
      mk_Do;
      mk_Done;
      mk_Downto;
      mk_Else;
      mk_End;
      mk_False;
      mk_For;
      mk_Fun;
      mk_If;
      mk_In;
      mk_Include;
      mk_Land;
      mk_Let;
      mk_Lor;
      mk_Lsl;
      mk_Lsr;
      mk_Lxor;
      mk_Match;
      mk_Mod;
      mk_Module;
      mk_Mut;
      mk_Not;
      mk_Of;
      mk_Or;
      mk_Rec;
      mk_Sig;
      mk_Struct;
      mk_Then;
      mk_True;
      mk_Type;
      mk_Val;
      mk_While;
      mk_With;

      mk_ContractOf;
      mk_ParameterOf;
      mk_Upto
    ]

    (* All keywords *)

    let keywords =
      let add map (key, data) =
        match SMap.add ~key ~data map with
          `Ok map -> map
        | `Duplicate -> map in
      let apply map mk_kwd =
        let lexemes = to_lexeme (mk_kwd Region.ghost) in
        List.fold_left ~f:(fun map lex -> add map (lex, mk_kwd))
                       ~init:map lexemes
      in List.fold_left ~f:apply ~init:SMap.empty keywords

    (* Ghost keywords *)

    (* OCaml keywords *)

    let ghost_begin   = wrap_begin   Region.ghost
    let ghost_do      = wrap_do      Region.ghost
    let ghost_done    = wrap_done    Region.ghost
    let ghost_downto  = wrap_downto  Region.ghost
    let ghost_else    = wrap_else    Region.ghost
    let ghost_end     = wrap_end     Region.ghost
    let ghost_false   = wrap_false   Region.ghost
    let ghost_for     = wrap_for     Region.ghost
    let ghost_fun     = wrap_fun     Region.ghost
    let ghost_if      = wrap_if      Region.ghost
    let ghost_in      = wrap_in      Region.ghost
    let ghost_include = wrap_include Region.ghost
    let ghost_land    = wrap_land    Region.ghost
    let ghost_let     = wrap_let     Region.ghost
    let ghost_lor     = wrap_lor     Region.ghost
    let ghost_lsl     = wrap_lsl     Region.ghost
    let ghost_lsr     = wrap_lsr     Region.ghost
    let ghost_lxor    = wrap_lxor    Region.ghost
    let ghost_match   = wrap_match   Region.ghost
    let ghost_mod     = wrap_mod     Region.ghost
    let ghost_module  = wrap_module  Region.ghost
    let ghost_mut     = wrap_mut     Region.ghost
    let ghost_not     = wrap_not     Region.ghost
    let ghost_of      = wrap_of      Region.ghost
    let ghost_or      = wrap_or      Region.ghost
    let ghost_rec     = wrap_rec     Region.ghost
    let ghost_sig     = wrap_sig     Region.ghost
    let ghost_struct  = wrap_struct  Region.ghost
    let ghost_then    = wrap_then    Region.ghost
    let ghost_true    = wrap_true    Region.ghost
    let ghost_type    = wrap_type    Region.ghost
    let ghost_val     = wrap_val     Region.ghost
    let ghost_while   = wrap_while   Region.ghost
    let ghost_with    = wrap_with    Region.ghost

    let ghost_Begin   = Begin   ghost_begin
    let ghost_Do      = Do      ghost_do
    let ghost_Done    = Done    ghost_done
    let ghost_Downto  = Downto  ghost_downto
    let ghost_Else    = Else    ghost_else
    let ghost_End     = End     ghost_end
    let ghost_False   = False   ghost_false
    let ghost_Fun     = Fun     ghost_fun
    let ghost_For     = For     ghost_for
    let ghost_If      = If      ghost_if
    let ghost_In      = In      ghost_in
    let ghost_Include = Include ghost_include
    let ghost_Land    = Land    ghost_land
    let ghost_Let     = Let     ghost_let
    let ghost_Lor     = Lor     ghost_lor
    let ghost_Lsl     = Lsl     ghost_lsl
    let ghost_Lsr     = Lsr     ghost_lsr
    let ghost_Lxor    = Lxor    ghost_lxor
    let ghost_Match   = Match   ghost_match
    let ghost_Mod     = Mod     ghost_mod
    let ghost_Module  = Module  ghost_module
    let ghost_Mut     = Mut     ghost_mut
    let ghost_Not     = Not     ghost_not
    let ghost_Of      = Of      ghost_of
    let ghost_Or      = Or      ghost_or
    let ghost_Rec     = Rec     ghost_rec
    let ghost_Struct  = Struct  ghost_struct
    let ghost_Then    = Then    ghost_then
    let ghost_True    = True    ghost_true
    let ghost_Type    = Type    ghost_type
    let ghost_While   = While   ghost_while
    let ghost_With    = With    ghost_with
    let ghost_Val     = Val     ghost_val
    let ghost_Sig     = Sig     ghost_sig

    (* CameLIGO-specific keywords *)

    let ghost_contract_of  = wrap_contract_of  Region.ghost
    let ghost_parameter_of = wrap_parameter_of Region.ghost
    let ghost_upto         = wrap_upto         Region.ghost

    let ghost_ContractOf  = ContractOf  ghost_contract_of
    let ghost_ParameterOf = ParameterOf ghost_parameter_of
    let ghost_Upto        = Upto        ghost_upto


    (* SYMBOLS *)

    let wrap_arrow    = wrap "->"
    let wrap_ass      = wrap ":="
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
    let wrap_plus_eq  = wrap "+="
    let wrap_minus_eq = wrap "-="
    let wrap_times_eq = wrap "*="
    let wrap_slash_eq = wrap "/="
    let wrap_vbar_eq  = wrap "|="

    (* Smart constructors *)

    let mk_ARROW    region = ARROW    (wrap_arrow    region)
    let mk_ASS      region = ASS      (wrap_ass      region)
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
    let mk_BOOL_OR  region = BOOL_OR  (wrap_bool_or  region)
    let mk_BOOL_AND region = BOOL_AND (wrap_bool_and region)
    let mk_QUOTE    region = QUOTE    (wrap_quote    region)
    let mk_REV_APP  region = REV_APP  (wrap_rev_app  region)

    (* All symbol smart constructors *)

    let symbols = [
      mk_ARROW;
      mk_ASS;
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
      let apply map mk_sym =
        let lexemes = to_lexeme (mk_sym Region.ghost) in
        List.fold_left ~f:(fun map lex -> add map (lex, mk_sym))
                       ~init:map lexemes
      in List.fold_left ~f:apply ~init:SMap.empty symbols

    (* Ghost symbols *)

    let ghost_arrow    = wrap_arrow    Region.ghost
    let ghost_ass      = wrap_arrow    Region.ghost
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

    let ghost_ARROW    = ARROW    ghost_arrow
    let ghost_ASS      = ASS      ghost_ass
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
    let ghost_BOOL_OR  = BOOL_OR  ghost_bool_or
    let ghost_BOOL_AND = BOOL_AND ghost_bool_and
    let ghost_QUOTE    = QUOTE    ghost_quote
    let ghost_REV_APP  = REV_APP  ghost_rev_app


    (* OTHER GHOST TOKENS *)

    (* IMPORTANT: These values cannot be exported in Token.mli *)

    let wrap_string   s = wrap s
    let wrap_verbatim s = wrap s
    let wrap_bytes    b = wrap ("0x" ^ Hex.show b, b)
    let wrap_int      z = wrap (Z.to_string z, z)
    let wrap_nat      z = wrap (Z.to_string z ^ "n", z)
    let wrap_mutez    m = wrap (Int64.to_string m ^ "mutez", m)
    let wrap_ident    i = wrap i
    let wrap_uident   i = wrap i
    let wrap_eident   i = wrap i

    let wrap_attr key value region = wrap (key, value) region

    let wrap_lang lang region : lexeme Region.reg Wrap.t =
      let start = region#start#shift_bytes (String.length "[%") in
      let lang_reg = Region.make ~start ~stop:region#stop
      in  wrap Region.{value=lang; region=lang_reg} region

    let ghost_string   s = wrap_string   s   Region.ghost
    let ghost_verbatim s = wrap_verbatim s   Region.ghost
    let ghost_bytes    b = wrap_bytes    b   Region.ghost
    let ghost_int      z = wrap_int      z   Region.ghost
    let ghost_nat      z = wrap_nat      z   Region.ghost
    let ghost_mutez    m = wrap_mutez    m   Region.ghost
    let ghost_ident    i = wrap_ident    i   Region.ghost
    let ghost_uident   i = wrap_uident   i   Region.ghost
    let ghost_eident   i = wrap_eident   i   Region.ghost
    let ghost_attr   k v = wrap_attr     k v Region.ghost
    let ghost_lang     l = wrap_lang     l   Region.ghost

    let ghost_String   s = String   (ghost_string s)
    let ghost_Verbatim s = Verbatim (ghost_verbatim s)
    let ghost_Bytes    b = Bytes    (ghost_bytes b)
    let ghost_Int      z = Int      (ghost_int z)
    let ghost_Nat      z = Nat      (ghost_nat z)
    let ghost_Mutez    m = Mutez    (ghost_mutez m)
    let ghost_Ident    i = Ident    (ghost_ident i)
    let ghost_UIdent   i = UIdent   (ghost_uident i)
    let ghost_EIdent   i = EIdent   (ghost_eident i)
    let ghost_Attr   k v = Attr     (ghost_attr k v)
    let ghost_Lang     l = Lang     (ghost_lang l)

    (* COMMENTS *)

    let wrap_block_com  c    = wrap c
    let ghost_block_com c    = wrap_block_com c Region.ghost
    let mk_BlockCom c region = BlockCom (wrap_block_com c region)
    let ghost_BlockCom c     = mk_BlockCom c Region.ghost

    let wrap_line_com c     = wrap c
    let ghost_line_com c    = wrap_line_com c Region.ghost
    let mk_LineCom c region = LineCom (wrap_line_com c region)
    let ghost_LineCom c     = mk_LineCom c Region.ghost

    (* VIRTUAL TOKENS *)

    let wrap_zwsp      = wrap ""
    let ghost_zwsp     = wrap_zwsp Region.ghost
    let mk_ZWSP region = ZWSP (wrap_zwsp region)
    let ghost_ZWSP     = mk_ZWSP Region.ghost

    (* END-OF-FILE TOKEN *)

    let wrap_eof      = wrap ""
    let mk_EOF region = EOF (wrap_eof region)
    let ghost_eof     = wrap_eof Region.ghost
    let ghost_EOF     = mk_EOF Region.ghost


    (* FROM TOKEN STRINGS TO LEXEMES *)

    let concrete = function
      "Ident"    -> "x"
    | "UIdent"   -> "X"
    | "EIdent"   -> "@x"
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
    | "ASS"      -> ghost_ass#payload
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
    | "BOOL_OR"  -> ghost_bool_or#payload
    | "BOOL_AND" -> ghost_bool_and#payload
    | "QUOTE"    -> ghost_quote#payload
    | "REV_APP"  -> ghost_rev_app#payload

    (* OCaml keywords *)

    | "Begin"   -> ghost_begin#payload
    | "Do"      -> ghost_do#payload
    | "Done"    -> ghost_done#payload
    | "Downto"  -> ghost_downto#payload
    | "Else"    -> ghost_else#payload
    | "End"     -> ghost_end#payload
    | "False"   -> ghost_false#payload
    | "For"     -> ghost_for#payload
    | "Fun"     -> ghost_fun#payload
    | "If"      -> ghost_if#payload
    | "In"      -> ghost_in#payload
    | "Include" -> ghost_include#payload
    | "Land"    -> ghost_land#payload
    | "Let"     -> ghost_let#payload
    | "Lor"     -> ghost_lor#payload
    | "Lsl"     -> ghost_lsl#payload
    | "Lsr"     -> ghost_lsr#payload
    | "Lxor"    -> ghost_lxor#payload
    | "Match"   -> ghost_match#payload
    | "Mod"     -> ghost_mod#payload
    | "Module"  -> ghost_module#payload
    | "Mut"     -> ghost_mut#payload
    | "Not"     -> ghost_not#payload
    | "Of"      -> ghost_of#payload
    | "Or"      -> ghost_or#payload
    | "Rec"     -> ghost_rec#payload
    | "Struct"  -> ghost_struct#payload
    | "Then"    -> ghost_then#payload
    | "True"    -> ghost_true#payload
    | "Type"    -> ghost_type#payload
    | "While"   -> ghost_while#payload
    | "With"    -> ghost_with#payload
    | "Val"     -> ghost_val#payload
    | "Sig"     -> ghost_sig#payload

    (* CameLIGO-specific keywords *)

    | "ContractOf"  -> ghost_contract_of#payload
    | "ParameterOf" -> ghost_parameter_of#payload
    | "Upto"        -> ghost_upto#payload

    (* Virtual tokens *)

    | "ZWSP" -> ""

    (* End-Of-File *)

    | "EOF" -> ghost_eof#payload

    (* This case should not happen! *)

    | _  -> "\\Unknown" (* Backslash meant to trigger an error *)


    (* FROM TOKENS TO TOKEN STRINGS AND REGIONS *)

    let comments (w : _ Wrap.t) =
      if Caml.(w#comments = [] && w#line_comment = None) then ""
      else " + comment(s)"

    let proj_token = function
      (* Preprocessing directives *)

      Directive d -> Directive.project d

      (* Comments *)

    | LineCom t ->
        t#region, sprintf "LineCom %S" t#payload
    | BlockCom t ->
        t#region, sprintf "BlockCom %S" t#payload

      (* Literals *)

    | String t ->
        t#region, sprintf "String %S%s" t#payload (comments t)
    | Verbatim t ->
        t#region, sprintf "Verbatim %S%s" t#payload (comments t)
    | Bytes t ->
        let s, b = t#payload in
        t#region,
        sprintf "Bytes (%S, \"0x%s\")%s" s (Hex.show b) (comments t)
    | Int t ->
        let s, n = t#payload in
        t#region, sprintf "Int (%S, %s)%s"
                          s (Z.to_string n) (comments t)
    | Nat t ->
        let s, n = t#payload in
        t#region, sprintf "Nat (%S, %s)" s (Z.to_string n)
    | Mutez t ->
        let s, n = t#payload in
        t#region, sprintf "Mutez (%S, %s)" s (Int64.to_string n)
    | Ident t ->
        t#region, sprintf "Ident %S%s" t#payload (comments t)
    | UIdent t ->
        t#region, sprintf "UIdent %S%s" t#payload (comments t)
    | EIdent t ->
        t#region, sprintf "EIdent %S%s" t#payload (comments t)
    | Attr t ->
       t#region, sprintf "Attr %s%s"
         (Attr.to_string t#payload) (comments t)
    | Lang t ->
        t#region, sprintf "Lang %S" t#payload.value

    (* Symbols *)

    | ARROW    t -> t#region, sprintf "ARROW%s" (comments t)
    | ASS      t -> t#region, sprintf "ASS%s" (comments t)
    | CONS     t -> t#region, sprintf "CONS%s" (comments t)
    | CARET    t -> t#region, sprintf "CARET%s" (comments t)
    | MINUS    t -> t#region, sprintf "MINUS%s" (comments t)
    | PLUS     t -> t#region, sprintf "PLUS%s" (comments t)
    | SLASH    t -> t#region, sprintf "SLASH%s" (comments t)
    | TIMES    t -> t#region, sprintf "TIMES%s" (comments t)
    | LPAR     t -> t#region, sprintf "LPAR%s" (comments t)
    | RPAR     t -> t#region, sprintf "RPAR%s" (comments t)
    | LBRACKET t -> t#region, sprintf "LBRACKET%s" (comments t)
    | RBRACKET t -> t#region, sprintf "RBRACKET%s" (comments t)
    | LBRACE   t -> t#region, sprintf "LBRACE%s" (comments t)
    | RBRACE   t -> t#region, sprintf "RBRACE%s" (comments t)
    | COMMA    t -> t#region, sprintf "COMMA%s" (comments t)
    | SEMI     t -> t#region, sprintf "SEMI%s" (comments t)
    | VBAR     t -> t#region, sprintf "VBAR%s" (comments t)
    | COLON    t -> t#region, sprintf "COLON%s" (comments t)
    | DOT      t -> t#region, sprintf "DOT%s" (comments t)
    | WILD     t -> t#region, sprintf "WILD%s" (comments t)
    | EQ       t -> t#region, sprintf "EQ%s" (comments t)
    | NE       t -> t#region, sprintf "NE%s" (comments t)
    | LT       t -> t#region, sprintf "LT%s" (comments t)
    | GT       t -> t#region, sprintf "GT%s" (comments t)
    | LE       t -> t#region, sprintf "LE%s" (comments t)
    | BOOL_OR  t -> t#region, sprintf "BOOL_OR%s" (comments t)
    | BOOL_AND t -> t#region, sprintf "BOOL_AND%s" (comments t)
    | QUOTE    t -> t#region, sprintf "QUOTE%s" (comments t)
    | REV_APP  t -> t#region, sprintf "REV_APP%s" (comments t)
    | PLUS_EQ  t -> t#region, sprintf "PLUS_EQ%s" (comments t)
    | MINUS_EQ t -> t#region, sprintf "MINUS_EQ%s" (comments t)
    | TIMES_EQ t -> t#region, sprintf "TIMES_EQ%s" (comments t)
    | SLASH_EQ t -> t#region, sprintf "SLASH_EQ%s" (comments t)
    | VBAR_EQ  t -> t#region, sprintf "VBAR_EQ%s" (comments t)

    (* OCaml keywords *)

    | Begin   t -> t#region, sprintf "Begin%s" (comments t)
    | Do      t -> t#region, sprintf "Do%s" (comments t)
    | Done    t -> t#region, sprintf "Done%s" (comments t)
    | Downto  t -> t#region, sprintf "Downto%s" (comments t)
    | Else    t -> t#region, sprintf "Else%s" (comments t)
    | End     t -> t#region, sprintf "End%s" (comments t)
    | False   t -> t#region, sprintf "False%s" (comments t)
    | For     t -> t#region, sprintf "For%s" (comments t)
    | Fun     t -> t#region, sprintf "Fun%s" (comments t)
    | If      t -> t#region, sprintf "If%s" (comments t)
    | In      t -> t#region, sprintf "In%s" (comments t)
    | Land    t -> t#region, sprintf "Land%s" (comments t)
    | Let     t -> t#region, sprintf "Let%s" (comments t)
    | Lor     t -> t#region, sprintf "Lor%s" (comments t)
    | Lsl     t -> t#region, sprintf "Lsl%s" (comments t)
    | Lsr     t -> t#region, sprintf "Lsr%s" (comments t)
    | Lxor    t -> t#region, sprintf "Lxor%s" (comments t)
    | Match   t -> t#region, sprintf "Match%s" (comments t)
    | Mod     t -> t#region, sprintf "Mod%s" (comments t)
    | Module  t -> t#region, sprintf "Module%s" (comments t)
    | Include t -> t#region, sprintf "Include%s" (comments t)
    | Mut     t -> t#region, sprintf "Mut%s" (comments t)
    | Not     t -> t#region, sprintf "Not%s" (comments t)
    | Of      t -> t#region, sprintf "Of%s" (comments t)
    | Or      t -> t#region, sprintf "Or%s" (comments t)
    | Val     t -> t#region, sprintf "Val%s" (comments t)
    | Sig     t -> t#region, sprintf "Sig%s" (comments t)
    | Rec     t -> t#region, sprintf "Rec%s" (comments t)
    | Struct  t -> t#region, sprintf "Struct%s" (comments t)
    | Then    t -> t#region, sprintf "Then%s" (comments t)
    | True    t -> t#region, sprintf "True%s" (comments t)
    | Type    t -> t#region, sprintf "Type%s" (comments t)
    | While   t -> t#region, sprintf "While%s" (comments t)
    | With    t -> t#region, sprintf "With%s" (comments t)

    (* CameLIGO-specific keywords *)

    | ContractOf  t -> t#region, sprintf "ContractOf%s" (comments t)
    | ParameterOf t -> t#region, sprintf "ParameterOf%s" (comments t)
    | Upto        t -> t#region, sprintf "Upto%s" (comments t)

    (* Virtual tokens *)

    | ZWSP   t -> t#region, sprintf "ZWSP"

    (* End-Of-File *)

    | EOF t -> t#region, sprintf "EOF"


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

    (* Escaped identifiers *)

    let mk_eident value region = EIdent (wrap value region)

    (* Constructors/Modules *)

    let mk_uident value region = UIdent (wrap value region)

    (* Attributes *)

    let mk_attr ~key ?value region = Attr (wrap (key, value) region)

    (* Code injection *)

    type lang_err = Wrong_lang_syntax of string (* Not CameLIGO *)

    let mk_lang lang region = Ok (Lang (wrap lang region))

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
    | BOOL_OR _
    | BOOL_AND _ -> true
    | _ -> false

    (* Verbatim, strings *)

    let verbatim_delimiters = ("{|", "|}")
  end

include T

module type S = module type of T
