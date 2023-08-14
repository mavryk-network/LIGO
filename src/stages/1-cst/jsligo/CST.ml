(* Concrete Syntax Tree (CST) for JsLIGO *)

(* Disabling warnings *)

[@@@warning "-30"] (* multiply-defined record labels *)

(* Vendor dependencies *)

module Directive = Preprocessor.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region
module Token     = Lexing_jsligo.Token

(* Local dependencies *)

module Wrap  = Lexing_shared.Wrap
module Attr  = Lexing_shared.Attr
module Nodes = Cst_shared.Nodes

(* Utilities *)

type 'a reg = 'a Region.reg
type 'payload wrap = 'payload Wrap.t

open Utils

type lexeme = string

(* Keywords of JsLIGO *)

(* IMPORTANT: The keywords are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

type kwd_as           = lexeme wrap
type kwd_break        = lexeme wrap
type kwd_case         = lexeme wrap
type kwd_const        = lexeme wrap
type kwd_contract_of  = lexeme wrap
type kwd_default      = lexeme wrap
type kwd_else         = lexeme wrap
type kwd_export       = lexeme wrap
type kwd_for          = lexeme wrap
type kwd_from         = lexeme wrap
type kwd_function     = lexeme wrap
type kwd_if           = lexeme wrap
type kwd_implements   = lexeme wrap
type kwd_import       = lexeme wrap
type kwd_interface    = lexeme wrap
type kwd_let          = lexeme wrap
type kwd_namespace    = lexeme wrap
type kwd_of           = lexeme wrap
type kwd_parameter_of = lexeme wrap
type kwd_return       = lexeme wrap
type kwd_switch       = lexeme wrap
type kwd_type         = lexeme wrap
type kwd_while        = lexeme wrap

(* Symbols *)

type arrow      = lexeme wrap  (* =>  *)
type dot        = lexeme wrap  (* .   *)
type ellipsis   = lexeme wrap  (* ... *)
type equal      = lexeme wrap  (* =   *)
type minus      = lexeme wrap  (* -   *)
type plus       = lexeme wrap  (* +   *)
type slash      = lexeme wrap  (* /   *)
type remainder  = lexeme wrap  (* %   *)
type times      = lexeme wrap  (* *   *)
type increment  = lexeme wrap  (* ++  *)
type decrement  = lexeme wrap  (* --  *)
type qmark      = lexeme wrap  (* ?   *)
type bit_and    = lexeme wrap  (* &   *)
type bit_neg    = lexeme wrap  (* ~   *)
type bit_or     = lexeme wrap  (* |   *)
type bit_xor    = lexeme wrap  (* ^   *)
type bit_sl     = lexeme wrap  (* <<  *)
type bit_sr     = lexeme wrap  (* >>  *)
type bool_or    = lexeme wrap  (* ||  *)
type bool_and   = lexeme wrap  (* &&  *)
type bool_xor   = lexeme wrap  (* ^^  *)
type bool_neg   = lexeme wrap  (* !   *)
type equal_cmp  = lexeme wrap  (* ==  *)
type neq        = lexeme wrap  (* !=  *)
type lt         = lexeme wrap  (* <   *)
type gt         = lexeme wrap  (* >   *)
type leq        = lexeme wrap  (* <=  *)
type geq        = lexeme wrap  (* >=  *)
type lpar       = lexeme wrap  (* (   *)
type rpar       = lexeme wrap  (* )   *)
type lbracket   = lexeme wrap  (* [   *)
type rbracket   = lexeme wrap  (* ]   *)
type lbrace     = lexeme wrap  (* {   *)
type rbrace     = lexeme wrap  (* }   *)
type comma      = lexeme wrap  (* ,   *)
type semi       = lexeme wrap  (* ;   *)
type vbar       = lexeme wrap  (* |   *)
type colon      = lexeme wrap  (* :   *)
type wild       = lexeme wrap  (* _   *)
type times_eq   = lexeme wrap  (* *=  *)
type div_eq     = lexeme wrap  (* /=  *)
type minus_eq   = lexeme wrap  (* -=  *)
type plus_eq    = lexeme wrap  (* +=  *)
type rem_eq     = lexeme wrap  (* %=  *)
type bit_sl_eq  = lexeme wrap  (* <<= *)
type bit_sr_eq  = lexeme wrap  (* >>= *)
type bit_and_eq = lexeme wrap  (* &=  *)
type bit_or_eq  = lexeme wrap  (* |=  *)
type bit_xor_eq = lexeme wrap  (* ^=  *)

type property_sep = lexeme wrap  (* , ; *)

(* End-Of-File *)

type eof = lexeme wrap

(* Literals *)

type variable       = lexeme wrap
type fun_name       = lexeme wrap
type type_name      = lexeme wrap
type type_var       = lexeme wrap
type type_ctor      = lexeme wrap
type ctor           = lexeme wrap
type property_name  = lexeme wrap
type namespace_name = lexeme wrap
type intf_name      = lexeme wrap
type file_path      = lexeme wrap
type language       = lexeme wrap
type attribute      = Attr.t wrap

type string_literal   = lexeme wrap
type int_literal      = (lexeme * Z.t) wrap
type nat_literal      = int_literal
type bytes_literal    = (lexeme * Hex.t) wrap
type mutez_literal    = (lexeme * Int64.t) wrap
type verbatim_literal = lexeme wrap

(* Parentheses, braces, brackets *)

type 'a par'      = {lpar: lpar; inside: 'a; rpar: rpar}
type 'a par       = 'a par' reg
type 'a braces'   = {lbrace: lbrace; inside: 'a; rbrace: rbrace}
type 'a braces    = 'a braces' reg
type 'a brackets' = {lbracket: lbracket; inside: 'a; rbracket: rbracket}
type 'a brackets  = 'a brackets' reg
type 'a chevrons' = {lchevron: lt; inside: 'a; rchevron: gt}
type 'a chevrons  = 'a chevrons' reg

(* The Abstract Syntax Tree *)

type t = {decl : top_decl nseq; eof : eof}

and cst = t

(* TOP-LEVEL DECLARATIONS *)

and top_decl =
  TL_Decl      of (declaration * semi option)
| TL_Attr      of (attribute * top_decl)
| TL_Export    of (kwd_export * top_decl) reg
| TL_Directive of Directive.t

(* INNER DECLARATIONS (AS STATEMENTS) *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and declaration =
  D_Fun       of fun_decl reg
| D_Import    of import_decl
| D_Interface of interface_decl reg
| D_Namespace of namespace_decl reg
| D_Type      of type_decl reg
| D_Value     of value_decl reg

(* Function declaration *)

and fun_decl = {
  kwd_function : kwd_function;
  fun_name     : fun_name;
  type_vars    : type_vars option;
  parameters   : fun_params;
  rhs_type     : type_annotation option;
  fun_body     : statements braces
}

and fun_params = (pattern, comma) sep_or_term par

(* Import declaration *)

and import_decl =
  ImportAlias of import_alias reg
| ImportAllAs of import_all_as reg
| ImportFrom  of import_from reg

and import_alias = {
  kwd_import     : kwd_import;
  alias          : namespace_name;
  equal          : equal;
  namespace_path : namespace_selection
}

and namespace_selection =
  M_Path  of namespace_name namespace_path reg
| M_Alias of namespace_name

and import_all_as = {
  kwd_import : kwd_import;
  times      : times;
  kwd_as     : kwd_as;
  alias      : namespace_name;
  kwd_from   : kwd_from;
  file_path  : file_path
}

and import_from = {
  kwd_import : kwd_import;
  imported   : (variable, comma) sep_or_term braces;
  kwd_from   : kwd_from;
  file_path  : file_path
}

(* Namespace paths *)

and 'a namespace_path = {
  namespace_path : (namespace_name, dot) nsepseq;
  selector    : dot;
  property       : 'a
}

(* Interfaces *)

and interface_decl = {
  kwd_interface : kwd_interface;
  intf_name     : intf_name;
  intf_body     : intf_body
}

and intf_body = intf_entries braces

and intf_entries = (intf_entry, semi) sep_or_term

and intf_entry =
  I_Attr  of (attribute * intf_entry)
| I_Type  of intf_type reg
| I_Const of intf_const reg

and intf_type = {
  kwd_type  : kwd_type;
  type_name : type_name;
  type_rhs  : (equal * type_expr) option
}

and intf_const = {
  kwd_const  : kwd_const;
  const_name : variable;
  const_type : type_annotation
}

(* Namespace declaration *)

and namespace_decl = {
  kwd_namespace  : kwd_namespace;
  namespace_name : namespace_name;
  namespace_type : interface option;
  namespace_body : statements braces
}

and interface = (kwd_implements * intf_expr) reg

and intf_expr =
  I_Body of intf_body
| I_Path of namespace_selection

(* Type declarations *)

and type_decl = {
  kwd_type  : kwd_type;
  name      : type_name;
  type_vars : type_vars option;
  eq        : equal;
  type_expr : type_expr
}

(* Value declaration *)

and value_decl = {
  kind     : var_kind;
  bindings : (val_binding reg, comma) nsepseq
}

and var_kind = [
  `Let   of kwd_let
| `Const of kwd_const
]

and val_binding = {
  pattern   : pattern;
  type_vars : type_vars option;
  rhs_type  : type_annotation option;
  eq        : equal;
  rhs_expr  : expr
}

and type_vars = (type_var, comma) sep_or_term chevrons

and type_annotation = colon * type_expr

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and type_expr =
  T_App       of (type_expr * type_ctor_args) reg  (* <u,v> M.t         *)
| T_Attr      of (attribute * type_expr)           (* @a e              *)
| T_Array     of array_type                        (* [t, [u, v]]       *)
| T_Fun       of fun_type                          (* (a : t) => u      *)
| T_Int       of int_literal                       (* 42                *)
| T_NamePath  of type_expr namespace_path reg      (* A.B.list<u>       *)
| T_Object    of type_expr _object                 (* {x; @a y : t}     *)
| T_Par       of type_expr par                     (* (t)               *)
| T_Parameter of parameter_of_type reg             (* parameter_of m    *)
| T_String    of string_literal                    (* "x"               *)
| T_Union     of union_type                        (* {kind: "C", x: t} *)
| T_Var       of variable                          (* t                 *)
| T_Variant   of variant_type                      (* ["A"] | ["B", t]  *)

(* Type application *)

and type_ctor_args = (type_expr, comma) nsep_or_term chevrons

(* Array type type *)

and array_type = (type_expr, comma) nsep_or_term brackets

(* Functional type *)

and fun_type        = (fun_type_params * arrow * type_expr) reg
and fun_type_params = (fun_type_param reg, comma) sep_or_term par
and fun_type_param  = variable * type_annotation

(* Parameter of type *)

and parameter_of_type = {
  kwd_parameter_of : kwd_parameter_of;
  namespace_path   : namespace_selection
}

(* Object type *)

and 'a _object = ('a property reg, property_sep) sep_or_term braces

and 'a property = {
  attributes : attribute list;
  property_id   : property_id;
  property_rhs  : (colon * 'a) option (* [None] means punning *)
}

and property_id =
  F_Int  of int_literal
| F_Name of property_name
| F_Str  of string_literal

(* Discriminated unions *)

and union_type = (type_expr _object, vbar) nsep_or_pref reg

(* Variant type *)

and variant_type = (variant, vbar) nsep_or_pref reg

and variant = {
  attributes : attribute list;
  tuple      : variant_comp brackets
}

and variant_comp = {
  ctor        : ctor;
  ctor_params : (comma * (type_expr, comma) nsep_or_term) option
}

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and pattern =
  P_Array    of pattern _array           (* [x, ...y, z] [] *)
| P_Attr     of (attribute * pattern)    (* @a [x, _]       *)
| P_Bytes    of bytes_literal            (* 0xFFFA          *)
| P_Ctor     of ctor                     (* C               *)
| P_Int      of int_literal              (* 42              *)
| P_Mutez    of mutez_literal            (* 5mutez          *)
| P_Nat      of nat_literal              (* 4n              *)
| P_Object   of pattern _object          (* {x, y : 0}      *)
| P_String   of string_literal           (* "string"        *)
| P_Typed    of typed_pattern reg        (* [x,y] : t       *)
| P_Var      of variable                 (* x               *)
| P_Verbatim of verbatim_literal         (* {|foo|}         *)

(* Tuple *)

and 'a _array = ('a component, comma) sep_or_term brackets

and 'a component = ellipsis option * 'a

(* Typed patterns (function parameters) *)

and typed_pattern = pattern * type_annotation

(* STATEMENTS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and statement =
  S_Attr   of (attribute * statement)
| S_Block  of statements braces
| S_Break  of kwd_break
| S_Cond   of cond_stmt reg
| S_Decl   of declaration
| S_Export of export_stmt reg
| S_Expr   of expr
| S_For    of for_stmt reg
| S_ForOf  of for_of_stmt reg
| S_Return of return_stmt reg
| S_Switch of switch_stmt reg
| S_While  of while_stmt reg

and statements = (statement * semi option) nseq

(* Export statement *)

and export_stmt = kwd_export * declaration

(* Conditional statement *)

and cond_stmt = {
  kwd_if : kwd_if;
  test   : expr par;
  if_so  : statement * semi option;
  if_not : (kwd_else * statement) option
}

(* For-loops *)

and for_stmt = {
  kwd_for  : kwd_for;
  range    : range_for par;
  for_body : statement option
}

and range_for = {
  initialiser  : statement option;
  semi1        : semi;
  condition    : expr option;
  semi2        : semi;
  afterthought : (expr, comma) nsepseq option
}

(* For-of loops *)

and for_of_stmt = {
  kwd_for     : kwd_for;
  range       : range_of par;
  for_of_body : statement
}

and range_of = {
  index_kind : var_kind;
  index      : variable;
  kwd_of     : kwd_of;
  expr       : expr
}

(* Return statement *)

and return_stmt = kwd_return * expr option

(* Switch statement *)

and switch_stmt = {
  kwd_switch : kwd_switch;
  subject    : expr par;
  cases      : cases braces
}

and cases =
  AllCases of all_cases
| Default  of switch_default reg

and all_cases = switch_case reg nseq * switch_default reg option

and switch_case = {
  kwd_case  : kwd_case;
  expr      : expr;
  colon     : colon;
  case_body : statements option
}

and switch_default = {
  kwd_default  : kwd_default;
  colon        : colon;
  default_body : statements option
}

(* While-loop *)

and while_stmt = {
  kwd_while  : kwd_while;
  invariant  : expr par;
  while_body : statement
}

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and expr =
  E_Add      of plus bin_op reg         (* x + y             *)
| E_AddEq    of plus_eq bin_op reg      (* x += y            *)
| E_And      of bool_and bin_op reg     (* x && y            *)
| E_App      of (expr * arguments) reg  (* f(x)   Foo()      *)
| E_Array    of expr _array             (* [x, ...y, z]  []  *)
| E_Assign   of equal bin_op reg        (* x = y             *)
| E_Attr     of (attribute * expr)      (* @a [x, y]         *)
| E_BitAnd   of bit_and bin_op reg      (* x & y             *)
| E_BitAndEq of bit_and_eq bin_op reg   (* x &= y            *)
| E_BitNeg   of bit_neg un_op reg       (* ~x                *)
| E_BitOr    of bit_or bin_op reg       (* x | y             *)
| E_BitOrEq  of bit_or_eq bin_op reg    (* x |= y            *)
| E_BitSl    of bit_sl bin_op reg       (* x << y            *)
| E_BitSlEq  of bit_sl_eq bin_op reg    (* x <<= y           *)
| E_BitSr    of bit_sr bin_op reg       (* x >> y            *)
| E_BitSrEq  of bit_sr_eq bin_op reg    (* x >>= y           *)
| E_BitXor   of bit_xor bin_op reg      (* x ^ y             *)
| E_BitXorEq of bit_xor_eq bin_op reg   (* x ^= y            *)
| E_Bytes    of bytes_literal           (* 0xFFFA            *)
| E_CodeInj  of code_inj reg
| E_Contract of contract_of_expr reg    (* contract_of (M.N) *)
| E_Ctor     of ctor                    (* C                 *)
| E_Div      of slash bin_op reg        (* x / y             *)
| E_DivEq    of div_eq bin_op reg       (* x /= y            *)
| E_Equal    of equal_cmp bin_op reg    (* x == y            *)
| E_Fun      of fun_expr reg            (* (x : int) => e    *)
| E_Geq      of geq bin_op reg          (* x >= y            *)
| E_Gt       of gt bin_op reg           (* x > y             *)
| E_Int      of int_literal             (* 42                *)
| E_Leq      of leq bin_op reg          (* x <= y            *)
| E_Lt       of lt bin_op reg           (* x < y             *)
| E_MinusEq  of minus_eq bin_op reg     (* x -= y            *)
| E_NamePath of expr namespace_path reg (* M.N.x.0           *)
| E_Rem      of remainder bin_op reg    (* x % n             *)
| E_RemEq    of rem_eq bin_op reg       (* x %= y            *)
| E_Mult     of times bin_op reg        (* x * y             *)
| E_Mutez    of mutez_literal           (* 5mutez            *)
| E_Nat      of nat_literal             (* 42n               *)
| E_Neg      of minus un_op reg         (* -x                *)
| E_Neq      of neq bin_op reg          (* x != y            *)
| E_Not      of bool_neg un_op reg      (* !x                *)
| E_Object   of expr _object            (* {x : e, y}        *)
| E_Or       of bool_or bin_op reg      (* x || y            *)
| E_Par      of expr par                (* (x + y)           *)
| E_PostDecr of decrement un_op reg     (* x--               *)
| E_PostIncr of increment un_op reg     (* x++               *)
| E_PreDecr  of decrement un_op reg     (* --x               *)
| E_PreIncr  of increment un_op reg     (* ++x               *)
| E_Proj     of projection reg          (* e.x.1             *)
| E_String   of string_literal          (* "abcdef"          *)
| E_Sub      of minus bin_op reg        (* x - y             *)
| E_Ternary  of ternary reg             (* x ? y : z         *)
| E_TimesEq  of times_eq bin_op reg     (* x *= y            *)
| E_Typed    of typed_expr reg          (* e as t            *)
| E_Update   of update_expr braces      (* {...x, y : z}     *)
| E_Var      of variable                (* x                 *)
| E_Verbatim of verbatim_literal        (* {|foo|}           *)
| E_Xor      of bool_xor bin_op reg     (* x ^^ y            *)

(* Applications *)

and arguments = (expr, comma) sepseq par

(* Functional expressions *)

and fun_expr = {
  type_vars  : type_vars option;
  parameters : arrow_fun_params;
  rhs_type   : type_annotation option;
  arrow      : arrow;
  fun_body   : fun_body
}

and arrow_fun_params =
  ParParams  of fun_params
| NakedParam of pattern

and fun_body =
  StmtBody of statements braces
| ExprBody of expr

(* Contract of expression *)

and contract_of_expr = {
  kwd_contract_of : kwd_contract_of;
  namespace_path  : namespace_selection par
}

(* Functional update of object expressions *)

and update_expr = {
  ellipsis : ellipsis;
  _object  : expr;
  sep      : property_sep;
  updates  : (expr property reg, property_sep) sep_or_term
}

(* Ternary conditional *)

and ternary = {
  condition : expr;
  qmark     : qmark;
  truthy    : expr;
  colon     : colon;
  falsy     : expr
}

(* Typed expression *)

and typed_expr = expr * kwd_as * type_expr

(* Binary and unary arithmetic operators *)

and 'a bin_op = {arg1: expr; op: 'a; arg2: expr}
and 'a  un_op = {op: 'a; arg: expr}

(* Projections *)

and projection = {
  object_or_array : expr;
  property_path      : selection nseq
}

and selection =
  FieldName of (dot * property_name)      (* Objects *)
| FieldStr  of string_literal brackets (* Objects *)
| Component of int_literal brackets    (* Arrays  *)

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : language;
  code     : expr
}

(* PROJECTIONS *)

(* Projecting regions from some nodes of the AST *)

let import_decl_to_region = function
  ImportAlias {region; _}
| ImportAllAs {region; _}
| ImportFrom  {region; _} -> region

let declaration_to_region = function
  D_Fun       {region; _} -> region
| D_Import    d -> import_decl_to_region d
| D_Interface {region; _}
| D_Namespace {region; _}
| D_Type      {region; _} -> region
| D_Value     {region; _} -> region

let rec top_decl_to_region = function
  TL_Decl      (d, _) -> declaration_to_region d
| TL_Attr      (_, d) -> top_decl_to_region d
| TL_Export    {region; _} -> region
| TL_Directive d -> Directive.to_region d

let rec type_expr_to_region = function
  T_App       {region; _}
| T_Array     {region; _} -> region
| T_Attr      (_, t) -> type_expr_to_region t
| T_Fun       {region; _} -> region
| T_Int       w -> w#region
| T_NamePath  {region; _}
| T_Object    {region; _}
| T_Par       {region; _}
| T_Parameter {region; _} -> region
| T_String    w -> w#region
| T_Union     {region; _} -> region
| T_Var       w -> w#region
| T_Variant   {region; _} -> region

let rec pattern_to_region = function
  P_Array {region; _} -> region
| P_Attr   (_, p) -> pattern_to_region p
| P_Bytes  w -> w#region
| P_Ctor   w -> w#region
| P_Int    w -> w#region
| P_Mutez  w -> w#region
| P_Nat    w -> w#region
| P_Object {region; _} -> region
| P_String w-> w#region
| P_Typed {region; _} -> region
| P_Var w -> w#region
| P_Verbatim w -> w#region

let rec expr_to_region = function
  E_Add      {region; _}
| E_AddEq    {region; _}
| E_And      {region; _}
| E_App      {region; _}
| E_Array    {region; _}
| E_Assign   {region; _} -> region
| E_Attr     (_, e) -> expr_to_region e
| E_BitAnd   {region; _}
| E_BitAndEq {region; _}
| E_BitNeg   {region; _}
| E_BitOr    {region; _}
| E_BitOrEq  {region; _}
| E_BitSl    {region; _}
| E_BitSlEq  {region; _}
| E_BitSr    {region; _}
| E_BitSrEq  {region; _}
| E_BitXor   {region; _}
| E_BitXorEq {region; _} -> region
| E_Bytes    w -> w#region
| E_CodeInj  {region; _}
| E_Contract {region; _} -> region
| E_Ctor     w -> w#region
| E_Div      {region; _}
| E_DivEq    {region; _}
| E_Equal    {region; _}
| E_Fun      {region; _}
| E_Geq      {region; _}
| E_Gt       {region; _} -> region
| E_Int      w -> w#region
| E_Leq      {region; _}
| E_Lt       {region; _}
| E_MinusEq  {region; _}
| E_NamePath {region; _}
| E_Rem      {region; _}
| E_RemEq    {region; _}
| E_Mult     {region; _} -> region
| E_Mutez    w -> w#region
| E_Nat      w -> w#region
| E_Neg      {region; _}
| E_Neq      {region; _}
| E_Not      {region; _}
| E_Object   {region; _}
| E_Or       {region; _}
| E_Par      {region; _}
| E_PostDecr {region; _}
| E_PostIncr {region; _}
| E_PreDecr  {region; _}
| E_PreIncr  {region; _}
| E_Proj     {region; _} -> region
| E_String   w -> w#region
| E_Sub      {region; _}
| E_Ternary  {region; _}
| E_TimesEq  {region; _}
| E_Typed    {region; _}
| E_Update   {region; _} -> region
| E_Var      w
| E_Verbatim w -> w#region
| E_Xor {region; _} -> region

let rec statement_to_region = function
  S_Attr   (_, s) -> statement_to_region s
| S_Block  {region; _} -> region
| S_Break  w -> w#region
| S_Cond   {region; _} -> region
| S_Decl   d -> declaration_to_region d
| S_Export {region; _} -> region
| S_Expr   e -> expr_to_region e
| S_For    {region; _}
| S_ForOf  {region; _}
| S_Return {region; _}
| S_Switch {region; _}
| S_While  {region; _} -> region

let var_kind_to_region = function
  `Let w | `Const w -> w#region

let property_id_to_region = function
  F_Name i -> i#region
| F_Int  i -> i#region
| F_Str  i -> i#region

let fun_body_to_region = function
  StmtBody {region; _} -> region
| ExprBody e -> expr_to_region e

let selection_to_region = function
  FieldName (dot, property_name) ->
    Region.cover dot#region property_name#region
| FieldStr brackets -> brackets.region
| Component brackets -> brackets.region

let namespace_selection_to_region = function
  M_Path {region; _} -> region
| M_Alias w -> w#region

let intf_expr_to_region = function
  I_Body {region; _} -> region
| I_Path path -> namespace_selection_to_region path

let parameters_to_region = function
  ParParams {region; _} -> region
| NakedParam p -> pattern_to_region p
