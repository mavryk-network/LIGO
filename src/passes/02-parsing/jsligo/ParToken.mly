(* Note: All external symbols here should be unqualified because this file is used
         by [menhir] that does not always insert the [%{..%}] header. So we work
         around it by the [-open Module] option in [dune] but symbols should be
         unqualified.

         Also, keep in mind that [ParToken.mly] and [Parser.mly] are merging into
         one file and the header of [Parser.mly] affects this code.
         For example: [lexeme] type comes from [open CST] *)

%[@recover.prelude
  (* See [dune] file for [-open] flags for modules used in the
     semantic value of tokens, like [Wrap]. *)

  module Directive = LexerLib.Directive
  module Region    = Simple_utils.Region
  module Token     = Lexing_jsligo.Token

  let mk_Directive region =
    let value = (1, "file_path", None)
    in Directive.Linemarker Region.{value; region}

 (*
  let mk_lang region =
    Region.{value = {value = "Ghost_lang"; region}; region} *)

  (* Ghost semantic values for inserted tokens *)

  let mk_string     loc = Token.wrap_string    "ghost string" loc
  let mk_verbatim   loc = Token.wrap_verbatim  "ghost verbatim" loc
  let mk_bytes      loc = Token.wrap_bytes     (Hex.of_string "Ghost bytes") loc
  let mk_int        loc = Token.wrap_int       Z.zero loc
(*let mk_nat        loc = Token.wrap_nat       Z.zero loc
  let mk_mutez      loc = Token.wrap_mutez     Int64.zero) *)
  let mk_ident      loc = Token.wrap_ident     "ghost_ident" loc
  let mk_uident     loc = Token.wrap_uident    "Ghost_uident" loc
  let mk_attr       loc = Token.wrap_attr      "ghost_attr" loc
  let mk_block_com  loc = Token.wrap_block_com "(* comment *)" loc
  let mk_line_com   loc = Token.wrap_line_com  "// comment" loc
]

(* Literals *)

%token             <string Wrap.t> BlockCom  "<block_comment>" [@recover.expr mk_block_com $loc]
%token             <string Wrap.t> LineCom   "<line_comment>"  [@recover.expr mk_line_com  $loc]

%token         <LexerLib.Directive.t> Directive "<directive>" [@recover.expr mk_Directive  $loc]
%token                <string Wrap.t> String    "<string>"    [@recover.expr mk_string     $loc]
%token                <string Wrap.t> Verbatim  "<verbatim>"  [@recover.expr mk_verbatim   $loc]
%token      <(string * Hex.t) Wrap.t> Bytes     "<bytes>"     [@recover.expr mk_bytes      $loc]
%token        <(string * Z.t) Wrap.t> Int       "<int>"       [@recover.expr mk_int        $loc]
%token                <string Wrap.t> Ident     "<ident>"     [@recover.expr mk_ident      $loc]
%token                <string Wrap.t> UIdent    "<uident>"    [@recover.expr mk_uident     $loc]
%token                <string Wrap.t> Attr      "[@attr]"     [@recover.expr mk_attr       $loc]
(*
%token        <(string * Z.t) Wrap.t> Nat       "<nat>"       [@recover.expr mk_nat        $loc]
%token        <(string * Z.t) Wrap.t> Mutez     "<mutez>"     [@recover.expr mk_mutez      $loc]
%token <string Region.reg Region.reg> Lang      "[%lang"      [@recover.expr mk_lang       $loc]
*)

(* Symbols *)

%token <string Wrap.t> MINUS   "-"    [@recover.expr Token.wrap_minus   $loc]
%token <string Wrap.t> PLUS    "+"    [@recover.expr Token.wrap_plus    $loc]
%token <string Wrap.t> SLASH   "/"    [@recover.expr Token.wrap_slash   $loc]
%token <string Wrap.t> TIMES   "*"    [@recover.expr Token.wrap_times   $loc]
%token <string Wrap.t> REM     "%"    [@recover.expr Token.wrap_rem     $loc]
(*
%token <string Wrap.t> PLUS2   "++"   [@recover.expr Token.wrap_plus2   $loc]
%token <string Wrap.t> MINUS2  "--"   [@recover.expr Token.wrap_minus2  $loc]
*)
%token <string Wrap.t> LPAR     "("   [@recover.expr Token.wrap_lpar     $loc]
%token <string Wrap.t> RPAR     ")"   [@recover.expr Token.wrap_rpar     $loc]
%token <string Wrap.t> LBRACKET "["   [@recover.expr Token.wrap_lbracket $loc]
%token <string Wrap.t> RBRACKET "]"   [@recover.expr Token.wrap_rbracket $loc]
%token <string Wrap.t> LBRACE   "{"   [@recover.expr Token.wrap_lbrace   $loc]
%token <string Wrap.t> RBRACE   "}"   [@recover.expr Token.wrap_rbrace   $loc]
%token <string Wrap.t> COMMA    ","   [@recover.expr Token.wrap_comma    $loc]
%token <string Wrap.t> SEMI     ";"   [@recover.expr Token.wrap_semi     $loc]
%token <string Wrap.t> COLON    ":"   [@recover.expr Token.wrap_colon    $loc]
%token <string Wrap.t> DOT      "."   [@recover.expr Token.wrap_dot      $loc]
%token <string Wrap.t> ELLIPSIS "..." [@recover.expr Token.wrap_ellipsis $loc]
%token <string Wrap.t> BOOL_OR  "||"  [@recover.expr Token.wrap_bool_or  $loc]
%token <string Wrap.t> BOOL_AND "&&"  [@recover.expr Token.wrap_bool_and $loc]
%token <string Wrap.t> BOOL_NOT "!"   [@recover.expr Token.wrap_bool_not $loc]
(*
%token <string Wrap.t> BIT_AND  "&"   [@recover.expr Token.wrap_bit_and  $loc]
%token <string Wrap.t> BIT_NOT  "~"   [@recover.expr Token.wrap_bit_not  $loc]
%token <string Wrap.t> BIT_XOR  "^"   [@recover.expr Token.wrap_bit_xor  $loc]
%token <string Wrap.t> SHIFT_L  "<<<" [@recover.expr Token.wrap_shift_l  $loc]
%token <string Wrap.t> SHIFT_R  ">>>" [@recover.expr Token.wrap_shift_r  $loc]
*)
%token <string Wrap.t> EQ       "="   [@recover.expr Token.wrap_eq       $loc]
%token <string Wrap.t> EQ2      "=="  [@recover.expr Token.wrap_eq2      $loc]
%token <string Wrap.t> NE       "!="  [@recover.expr Token.wrap_ne       $loc]
%token <string Wrap.t> LT       "<"   [@recover.expr Token.wrap_lt       $loc]
%token <string Wrap.t> GT       ">"   [@recover.expr Token.wrap_gt       $loc]
%token <string Wrap.t> LE       "<="  [@recover.expr Token.wrap_le       $loc]
%token <string Wrap.t> GE       ">="  [@recover.expr Token.wrap_ge       $loc]
%token <string Wrap.t> PLUS_EQ  "+="  [@recover.expr Token.wrap_plus_eq  $loc]
%token <string Wrap.t> MINUS_EQ "-="  [@recover.expr Token.wrap_minus_eq $loc]
%token <string Wrap.t> MULT_EQ  "*="  [@recover.expr Token.wrap_mult_eq  $loc]
%token <string Wrap.t> REM_EQ   "%="  [@recover.expr Token.wrap_rem_eq   $loc]
%token <string Wrap.t> DIV_EQ   "/="  [@recover.expr Token.wrap_div_eq   $loc]
(*
%token <string Wrap.t> SL_EQ    "<<<=" [@recover.expr Token.wrap_sl_eq    $loc]
%token <string Wrap.t> SR_EQ    ">>>=" [@recover.expr Token.wrap_sr_eq    $loc]
%token <string Wrap.t> AND_EQ   "&="   [@recover.expr Token.wrap_and_eq   $loc]
%token <string Wrap.t> OR_EQ    "|="   [@recover.expr Token.wrap_or_eq    $loc]
%token <string Wrap.t> XOR_EQ   "^="   [@recover.expr Token.wrap_xor_eq   $loc]
*)
%token <string Wrap.t> VBAR     "|"   [@recover.expr Token.wrap_vbar     $loc]
%token <string Wrap.t> ARROW    "=>"  [@recover.expr Token.wrap_arrow    $loc]
%token <string Wrap.t> WILD     "_"   [@recover.expr Token.wrap_wild     $loc]

(* JavaScript Keywords *)

%token <string Wrap.t> Break   "break"   [@recover.expr Token.wrap_break   $loc]
%token <string Wrap.t> Case    "case"    [@recover.expr Token.wrap_case    $loc]
%token <string Wrap.t> Const   "const"   [@recover.expr Token.wrap_const   $loc]
%token <string Wrap.t> Default "default" [@recover.expr Token.wrap_default $loc]
%token <string Wrap.t> Else    "else"    [@recover.expr Token.wrap_else    $loc]
%token <string Wrap.t> Export  "export"  [@recover.expr Token.wrap_export  $loc]
%token <string Wrap.t> For     "for"     [@recover.expr Token.wrap_for     $loc]
%token <string Wrap.t> If      "if"      [@recover.expr Token.wrap_if      $loc]
%token <string Wrap.t> Import  "import"  [@recover.expr Token.wrap_import  $loc]
%token <string Wrap.t> Let     "let"     [@recover.expr Token.wrap_let     $loc]
%token <string Wrap.t> Of      "of"      [@recover.expr Token.wrap_of      $loc]
%token <string Wrap.t> Return  "return"  [@recover.expr Token.wrap_return  $loc]
%token <string Wrap.t> Switch  "switch"  [@recover.expr Token.wrap_switch  $loc]
%token <string Wrap.t> While   "while"   [@recover.expr Token.wrap_while   $loc]

(* TypeScript keywords *)

%token <string Wrap.t> As        "as"        [@recover.expr Token.wrap_as        $loc]
%token <string Wrap.t> Namespace "namespace" [@recover.expr Token.wrap_namespace $loc]
%token <string Wrap.t> Type      "type"      [@recover.expr Token.wrap_type      $loc]

(* Virtual tokens *)

%token <string Wrap.t> ZWSP [@recover.expr Token.wrap_zwsp $loc]

(* End of File *)

%token <string Wrap.t> EOF [@recover.expr Token.wrap_eof $loc]

%%
