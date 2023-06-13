(* Concrete Syntax Tree (CST) for JsLIGO *)

(* Disabling warnings *)

[@@@warning "-30"] (* multiply-defined record labels *)
(* [@@@warning "-30-40-42"] *)

(* Vendor dependencies *)

module Directive = Preprocessor.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region
module Token     = Lexing_jsligo.Token

(* Local dependencies *)

module Wrap = Lexing_shared.Wrap
module Attr = Lexing_shared.Attr
module CommonNodes = Cst_shared.CommonNodes

(* Utilities *)

type 'a reg = 'a Region.reg
type 'payload wrap = 'payload Wrap.t

type ('a, 'sep) sep_or_term  = ('a, 'sep) CommonNodes.sep_or_term
type ('a, 'sep) nsep_or_term = ('a, 'sep) CommonNodes.nsep_or_term
type ('a, 'sep) nsep_or_pref = ('a, 'sep) CommonNodes.nsep_or_pref

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

type arrow     = lexeme wrap  (* =>  *)
type dot       = lexeme wrap  (* .   *)
type ellipsis  = lexeme wrap  (* ... *)
type equal     = lexeme wrap  (* =   *)
type minus     = lexeme wrap  (* -   *)
type plus      = lexeme wrap  (* +   *)
type slash     = lexeme wrap  (* /   *)
type modulo    = lexeme wrap  (* %   *)
type times     = lexeme wrap  (* *   *)
type increment = lexeme wrap  (* ++  *)
type decrement = lexeme wrap  (* --  *)
type qmark     = lexeme wrap  (* ?   *)
type bool_or   = lexeme wrap  (* ||  *)
type bool_and  = lexeme wrap  (* &&  *)
type negation  = lexeme wrap  (* !   *)
type equal_cmp = lexeme wrap  (* ==  *)
type neq       = lexeme wrap  (* !=  *)
type lt        = lexeme wrap  (* <   *)
type gt        = lexeme wrap  (* >   *)
type leq       = lexeme wrap  (* <=  *)
type geq       = lexeme wrap  (* >=  *)
type lpar      = lexeme wrap  (* (   *)
type rpar      = lexeme wrap  (* )   *)
type lbracket  = lexeme wrap  (* [   *)
type rbracket  = lexeme wrap  (* ]   *)
type lbrace    = lexeme wrap  (* {   *)
type rbrace    = lexeme wrap  (* }   *)
type comma     = lexeme wrap  (* ,   *)
type semi      = lexeme wrap  (* ;   *)
type vbar      = lexeme wrap  (* |   *)
type colon     = lexeme wrap  (* :   *)
type wild      = lexeme wrap  (* _   *)
type times_eq  = lexeme wrap  (* *=  *)
type div_eq    = lexeme wrap  (* /=  *)
type minus_eq  = lexeme wrap  (* -=  *)
type plus_eq   = lexeme wrap  (* +=  *)
type mod_eq    = lexeme wrap  (* %=  *)

type field_sep = lexeme wrap  (* , ; *)

(* End-Of-File *)

type eof = lexeme wrap

(* Literals *)

type variable    = lexeme wrap
type type_name   = lexeme wrap
type type_var    = lexeme wrap
type ctor        = lexeme wrap
type field_name  = lexeme wrap
type module_name = lexeme wrap
type intf_name   = lexeme wrap
type file_path   = lexeme wrap
type language    = lexeme wrap
type attribute   = Attr.t wrap

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

type t = {
  decl : top_decl nseq;
  eof  : eof
}

and cst = t

(* TOP-LEVEL DECLARATIONS *)

and top_decl =
  TL_Decl      of declaration * semi option
| TL_Attr      of (attribute * top_decl) reg
| TL_Export    of (kwd_export * top_decl) reg
| TL_Directive of Directive.t

(* INNER DECLARATIONS (AS STATEMENTS) *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and declaration =
  D_Value     of value_decl reg
| D_Import    of import_decl
| D_Interface of interface_decl reg
| D_Module    of module_decl reg
| D_Type      of type_decl reg

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
  binders     : pattern;
  type_params : type_params option;
  rhs_type    : type_annotation option;
  eq          : equal;
  rhs_expr    : expr
}

and type_params = (variable, comma) sep_or_term chevrons

and type_annotation = colon * type_expr

(* Import declaration *)

and import_decl =
  AliasModule of import_alias reg
| ImportAll   of import_all_as reg
| ImportSome  of import_from reg

and import_alias = {
  kwd_import  : kwd_import;
  alias       : module_name;
  equal       : equal;
  module_path : module_name module_path reg
}

and import_all_as = {
  kwd_import : kwd_import;
  times      : times;
  kwd_as     : kwd_as;
  alias      : module_name;
  kwd_from   : kwd_from;
  file_path  : file_path
}

and import_from = {
  kwd_import : kwd_import;
  imported   : (field_name, comma) sep_or_term braces;
  kwd_from   : kwd_from;
  file_path  : file_path
}

(* Module paths *)

and 'a module_path = {
  module_path : (module_name, dot) nsepseq;
  selector    : dot;
  field       : 'a
}

(* Interfaces *)

and interface_decl = {
  kwd_interface : kwd_interface;
  intf_name     : intf_name;
  intf_body     : intf_body
}

and intf_body = intf_entries braces

and intf_entries = (intf_entry, semi) nsep_or_term

and intf_entry =
  I_Attr    of (attribute * intf_entry) reg
| I_Type    of intf_type reg
| I_Const   of intf_const reg

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

(* Module declaration *)

and module_decl = {
  kwd_namespace : kwd_namespace;
  module_name   : module_name;
  module_type   : interface option;
  module_body   : statements braces
}

and interface = (kwd_implements * intf_expr) reg

and intf_expr =
  I_Body of intf_body
| I_Path of module_name module_path reg

(* Type declarations *)

and type_decl = {
  kwd_type  : kwd_type;
  params    : type_vars option;
  name      : type_name;
  eq        : equal;
  type_expr : type_expr
}

and type_vars = (type_var, comma) sep_or_term chevrons

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and type_expr =
  T_App       of (type_expr * type_ctor_args) reg  (* <u,v> M.t         *)
| T_Attr      of (attribute * type_expr) reg       (* @a e              *)
| T_Cart      of cartesian                         (* [t, [u, v]]       *)
| T_Fun       of fun_type                          (* (a : t) => u      *)
| T_Int       of (lexeme * Z.t) wrap               (* 42                *)
| T_ModPath   of type_expr module_path reg         (* A.B.[u, v]        *)
| T_Par       of type_expr par                     (* (t)               *)
| T_Parameter of parameter_of_type reg             (* parameter_of m    *)
| T_Record    of type_expr field record            (* {x; @a y : t}     *)
| T_String    of lexeme wrap                       (* "x"               *)
| T_Union     of union_type                        (* {kind: "C", x: t} *)
| T_Var       of variable                          (* t                 *)
| T_Variant   of variant_type reg                  (* ["A"] | ["B", t]  *)

(*  Type application *)

and type_ctor_args = (type_expr, comma) nsep_or_term chevrons

(* Cartesian type *)

and cartesian = (type_expr, comma) nsep_or_term brackets

(* Functional type *)

and fun_type = (parameters * arrow * type_expr) reg

and parameters = (pattern, comma) sep_or_term par

(* Parameter of type *)

and parameter_of_type = {
  kwd_parameter_of : kwd_parameter_of;
  module_path      : module_name module_path reg
}

(* Record type *)

and 'a record = ('a reg, field_sep) sep_or_term braces

and 'rhs field = {
  attributes : attribute list;
  field_name : field_name;
  field_rhs  : (colon * 'rhs) option
}

(* Variant type *)

and variant_type = (variant reg, vbar) nsep_or_pref reg

and variant = {
  attributes : attribute list;
  tuple      : variant_comp brackets
}

and variant_comp = {
  ctor        : ctor;
  ctor_params : (comma * (type_expr, comma) nsep_or_term) option
}

(* Discriminated unions *)

and union_type = (type_expr field record, vbar) nsep_or_pref reg

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and pattern =
  P_Attr   of (attribute * pattern) reg (* @a [x, _]      *)
| P_Record of pattern field record      (* {x, y : 0}     *)
| P_Tuple  of pattern tuple             (* [x, ...y, z]   *)
| P_Typed  of typed_pattern             (* [] : list<int> *)
| P_Var    of variable                  (* x              *)

(* Tuple *)

and 'a tuple = ('a component, comma) sep_or_term brackets

and 'a component = ellipsis option * 'a

(* Typed pattern *)

and typed_pattern = pattern * type_annotation

(* STATEMENTS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and statement =
  S_Attr   of (attribute * statement) reg
| S_Block  of statements braces
| S_Break  of kwd_break
| S_Cond   of cond_stmt reg
| S_Decl   of declaration
| S_Expr   of expr
| S_For    of for_stmt reg
| S_ForOf  of for_of_stmt reg
| S_Return of return_stmt reg
| S_Switch of switch_stmt reg
| S_While  of while_stmt reg

and statements = (statement, semi) nsep_or_term

(* Conditional statement *)

and cond_stmt = {
  kwd_if : kwd_if;
  test   : expr par;
  if_so  : statement;
  if_not : (kwd_else * statement) option
}

(* For-loops *)

and for_stmt = {
  kwd_for   : kwd_for;
  range     : range_for par;
  statement : statement option
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
  kwd_for   : kwd_for;
  range     : range_of par;
  statement : statement
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
  cases      : case nseq braces
}

and case =
  Case    of switch_case
| Default of switch_default (* Unicity not checked *)

and switch_case = {
  kwd_case   : kwd_case;
  expr       : expr;
  colon      : colon;
  statements : statements option
}

and switch_default = {
  kwd_default : kwd_default;
  colon       : colon;
  statements  : statements option
}

(* While-loop *)

and while_stmt = {
  kwd_while : kwd_while;
  invariant : expr par;
  statement : statement
}

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and expr =
  E_Add      of plus bin_op reg         (* x + y             *)
| E_AddEq    of plus_eq bin_op reg      (* x += y            *)
| E_And      of bool_and bin_op reg     (* x && y            *)
| E_App      of (expr * arguments) reg  (* f(x)   Foo()      *)
| E_Attr     of (attribute * expr) reg  (* @a [x, y]         *)
| E_Bytes    of (lexeme * Hex.t) wrap   (* 0xFFFA            *)
| E_CodeInj  of code_inj reg
| E_Contract of contract_of_expr reg    (* contract_of (M.N) *)
| E_Ctor     of ctor                    (* C                 *)
| E_Div      of slash bin_op reg        (* x / y             *)
| E_DivEq    of div_eq bin_op reg       (* x /= y            *)
| E_Equal    of equal_cmp bin_op reg    (* x == y            *)
| E_Fun      of fun_expr reg            (* (x : int) => e    *)
| E_Geq      of geq bin_op reg          (* x >= y            *)
| E_Gt       of gt bin_op reg           (* x > y             *)
| E_Int      of (lexeme * Z.t) wrap     (* 42                *)
| E_Leq      of leq bin_op reg          (* x <= y            *)
| E_Lt       of lt bin_op reg           (* x < y             *)
| E_MinusEq  of minus_eq bin_op reg     (* x -= y            *)
| E_Mod      of modulo bin_op reg       (* x % n             *)
| E_ModEq    of mod_eq bin_op reg       (* x %= y            *)
| E_ModPath  of expr module_path reg    (* M.N.x.0           *)
| E_Mult     of times bin_op reg        (* x * y             *)
| E_Neg      of minus un_op reg         (* -x                *)
| E_Neq      of neq bin_op reg          (* x != y            *)
| E_Not      of negation un_op reg      (* !x                *)
| E_Or       of bool_or bin_op reg      (* x || y            *)
| E_Record   of expr field record       (* {x : e, y}        *)
| E_Par      of expr par                (* (x + y)           *)
| E_PostDecr of decrement un_op reg     (* x--               *)
| E_PostIncr of increment un_op reg     (* x++               *)
| E_PreDecr  of decrement un_op reg     (* --x               *)
| E_PreIncr  of increment un_op reg     (* ++x               *)
| E_Proj     of projection reg          (* e.x.1             *)
| E_String   of lexeme wrap             (* "abcdef"          *)
| E_Sub      of minus bin_op reg        (* x - y             *)
| E_Ternary  of ternary reg             (* x ? y : z         *)
| E_TimesEq  of times_eq bin_op reg     (* x *= y            *)
| E_Tuple    of expr tuple              (* [x, ...y, z]      *)
| E_Typed    of typed_expr reg          (* e as t            *)
| E_Unit     of the_unit reg            (* ()                *)
| E_Update   of update_expr braces      (* {...x, y : z}     *)
| E_Var      of variable                (* x                 *)
| E_Verbatim of lexeme wrap             (* {|foo|}           *)

(*| E_Seq      of (expr, comma) nsepseq reg (* TODO: ??? *) *)

(* Applications *)

and arguments = (expr, comma) sepseq par

(* Functional expressions *)

and fun_expr = {
  type_params : type_params option;
  parameters  : parameters;
  rhs_type    : type_annotation option;
  arrow       : arrow;
  body        : body;
}

and body =
  FunBody  of statements braces
| ExprBody of expr

(* Contract of expression *)

and contract_of_expr = {
  kwd_contract_of : kwd_contract_of;
  module_path     : module_name module_path par
}

(* Functional update of record expressions *)

and update_expr = {
  ellipsis : ellipsis;
  record   : expr;
  comma    : comma;
  updates  : (expr field, semi) nsepseq
}

(* Unit value *)

and the_unit = lpar * rpar

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
  record_or_tuple : expr;
  field_path      : selection nseq
}

and selection =
  FieldName of dot * field_name
| Component of expr brackets

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : language;
  code     : expr;
}

(* PROJECTIONS *)

(* Projecting regions from some nodes of the AST *)

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nseq_to_region to_region (hd,tl) =
  Region.cover (to_region hd) (last to_region tl)

let nsepseq_to_region to_region (hd,tl) =
  let reg (_, item) = to_region item in
  Region.cover (to_region hd) (last reg tl)

let declaration_to_region = function
  D_Value     {region; _}
| D_Import    {region; _}
| D_Interface {region; _}
| D_Module    {region; _}
| D_Type      {region; _} -> region

let rec top_decl_to_region = function
  TL_Decl      (d, _) -> declaration_to_region d
| TL_Attr      {region; _}
| TL_Export    {region; _} -> region
| TL_Directive d -> Directive.to_region d

let rec type_expr_to_region = function
  T_App       {region; _}
| T_Attr      {region; _}
| T_Cart      {region; _}
| T_Fun       {region; _} -> region
| T_Int       w -> w#region
| T_ModPath   {region; _}
| T_Par       {region; _}
| T_Parameter {region; _}
| T_Record    {region; _} -> region
| T_String    w -> w#region
| T_Union     {region; _} -> region
| T_Var       w -> w#region
| T_Variant   {region; _} -> region

let rec pattern_to_region = function
  P_Attr   {region; _}
| P_Record {region; _}
| P_Tuple  {region; _} -> region
| P_Typed  (p, (_, e)) ->
    Region.cover (pattern_to_region p) (type_expr_to_region e)
| P_Var w -> w#region

let rec expr_to_region = function
  E_Add      {region; _}
| E_AddEq    {region; _}
| E_And      {region; _}
| E_App      {region; _} -> region
| E_Attr     (_, e) -> expr_to_region e
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
| E_Mod      {region; _}
| E_ModEq    {region; _}
| E_ModPath  {region; _}
| E_Mult     {region; _}
| E_Neg      {region; _}
| E_Neq      {region; _}
| E_Not      {region; _}
| E_Or       {region; _}
| E_Record   {region; _}
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
| E_Tuple    {region; _}
| E_Typed    {region; _}
| E_Unit     {region; _}
| E_Update   {region; _} -> region
| E_Var      w
| E_Verbatim w -> w#region

let rec statement_to_region = function
  S_Attr   {region; _}
| S_Block  {region; _} -> region
| S_Break  w -> w#region
| S_Cond   {region; _} -> region
| S_Decl   d -> declaration_to_region d
| S_Expr   e -> expr_to_region e
| S_For    {region; _}
| S_ForOf  {region; _}
| S_Return {region; _}
| S_Switch {region; _}
| S_While  {region; _} -> region
