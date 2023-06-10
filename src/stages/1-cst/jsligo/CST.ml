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

(* Utilities *)

type 'a reg = 'a Region.reg
type 'payload wrap = 'payload Wrap.t

open Utils

type lexeme = string

(* Keywords of JsLIGO *)

(* IMPORTANT: The keywords are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

type kwd_as         = lexeme wrap
type kwd_break      = lexeme wrap
type kwd_case       = lexeme wrap
type kwd_const      = lexeme wrap
type kwd_default    = lexeme wrap
type kwd_else       = lexeme wrap
type kwd_export     = lexeme wrap
type kwd_for        = lexeme wrap
type kwd_from       = lexeme wrap
type kwd_if         = lexeme wrap
type kwd_implements = lexeme wrap
type kwd_import     = lexeme wrap
type kwd_interface  = lexeme wrap
type kwd_let        = lexeme wrap
type kwd_namespace  = lexeme wrap
type kwd_of         = lexeme wrap
type kwd_return     = lexeme wrap
type kwd_switch     = lexeme wrap
type kwd_type       = lexeme wrap
type kwd_while      = lexeme wrap

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

type attribute    = Attr.t wrap
type language     = lexeme wrap

(* Parentheses, braces, brackets *)

type 'a par'      = {lpar: lpar; inside: 'a; rpar: rpar}
type 'a par       = 'a par' reg
type 'a braces'   = {lbrace: lbrace; inside: 'a; rbrace: rbrace}
type 'a braces    = 'a braces' reg
type 'a brackets' = {lbracket: lbracket; inside: 'a; rbracket: rbracket}
type 'a brackets  = 'a brackets' reg
type 'a chevrons' = {lchevron: lt; inside: 'a; rchevron: gt}
type 'a chevrons  = 'a chevrons' reg

(* Lists *)

type ('a, 'sep) sep_or_term =
  Sep  of ('a, 'sep) sepseq
| Term of ('a * 'sep) seq

type ('a, 'sep) nsep_or_term =
  NSep  of ('a, 'sep) nsepseq
| NTerm of ('a * 'sep) nseq

type ('a, 'sep) nsep_or_pref =
  NSep of ('a, 'sep) nsepseq
| Pref of ('sep * 'a) nseq

(* The Abstract Syntax Tree *)

type t = {
  decl : top_decl nseq;
  eof  : eof
}

and cst = t

(* TOP-LEVEL DECLARATIONS *)

and top_decl =
  TL_Decl      of declaration * semi option
| TL_Attr      of (attribute * top_decl)
| TL_Export    of kwd_export * top_decl
| TL_Directive of Directive.t

(* INNER DECLARATIONS (AS STATEMENTS) *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and declaration =
  D_Value     of value_decl reg
| D_Import    of import_decl reg
| D_Interface of interface_decl reg
| D_Module    of module_decl reg
| D_Type      of type_decl reg

(* Value declaration *)

and value_decl = {
  kind     : val_kind;
  bindings : (val_binding reg, comma) nsepseq
}

and val_kind = [
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
  Alias of import_alias
| All   of import_all_as
| From  of import_from

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
  I_Attr    of (attribute * intf_entry)
| I_Type    of intf_type reg
| I_Const   of intf_const reg

and intf_type = {
  kwd_type : kwd_type;
  name     : type_name;
  type_rhs : (equal * type_expr) option
}

and intf_const = {
  kwd_const  : kwd_const;
  name       : variable;
  const_type : type_annotation
}

(* Module declaration *)

and module_decl = {
  kwd_namespace : kwd_namespace;
  module_name   : module_name;
  module_body   : statements braces
}

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
| T_Attr      of (attribute * type_expr)           (* @a e              *)
| T_Cart      of cartesian                         (* [t, [u, v]]       *)
| T_Fun       of fun_type                          (* (a : t) => u      *)
| T_Int       of (lexeme * Z.t) wrap               (* 42                *)
| T_ModPath   of type_expr module_path reg         (* A.B.(t * u)       *)
| T_Par       of type_expr par                     (* (t)               *)
| T_Parameter of (module_name, dot) nsepseq reg    (* parameter_of m    *)
| T_Record    of type_expr record                  (* {x; @a y : t}     *)
| T_String    of lexeme wrap                       (* "x"               *)
| T_Union     of union_type                        (* {kind: "C", x: t} *)
| T_Var       of variable                          (* t                 *)
| T_Variant   of variant_type reg                  (* ["A"] | ["B", t]  *)

(*  Type application *)

and type_ctor_args = (type_expr, comma) nsep_or_term chevrons

(* Cartesian type *)

and cartesian = (type_expr, comma) nsep_or_term brackets

(* Functional type *)

and fun_type = (fun_type_params * arrow * type_expr) reg

and fun_type_params = (fun_type_param, comma) nsepseq par

and fun_type_param = variable * type_annotation option

(* Record type *)

and 'a record = ('a field reg, field_sep) sep_or_term braces

and 'rhs field =
  Full of 'rhs full_field
| Incl of ellipsis * variable (* Type or value depending on the context *)

and 'rhs full_field = {
  attributes : attribute list;
  field_name : field_name;
  field_rhs  : (colon * 'rhs) option
}

(* Variant type *)

and variant_type = (variant reg, vbar) nsep_or_pref

and variant = {
  attributes : attribute list;
  tuple      : variant_comp brackets
}

and variant_comp = {
  ctor        : ctor;
  ctor_params : (comma * (type_expr, comma) nsep_or_term) option
}

(* Discriminated unions *)

and union_type = (type_expr record, vbar) nsep_or_pref

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and pattern =
  P_Attr   of (attribute * pattern)  (* @a [x, _]      *)
| P_Record of pattern record         (* {x, y : 0}     *)
| P_Tuple  of pattern tuple          (* [x, ...y, z]   *)
| P_Typed  of typed_pattern          (* [] : list<int> *)
| P_Var    of variable               (* x              *)

(* Tuple *)

and 'a tuple = ('a component, comma) sep_or_term brackets

and 'a component = ellipsis option * 'a

(* Typed pattern *)

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
  index_kind : val_kind;
  index      : variable;
  kwd_of     : kwd_of;
  expr       : expr
}

(* Return statement *)

and return_stmt = kwd_return * expr option

(* Switch statement *)

and switch_stmt = {
  kwd_switch  : kwd_switch;
  test        : expr par;
  switch_body : case nseq braces
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
  cond      : expr par;
  statement : statement
}

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and expr =
  E_Add      of plus bin_op reg                (* x + y          *)
| E_AddEq    of plus_eq bin_op reg             (* x += y         *)
| E_And      of bool_and bin_op reg            (* x && y         *)
| E_App      of (expr * arguments) reg         (* f(x)   Foo()   *)
| E_Attr     of (attribute * expr)             (* @a [x, y]      *)
| E_Bytes    of (lexeme * Hex.t) wrap          (* 0xFFFA         *)
| E_CodeInj  of code_inj reg
| E_Contract of (module_name, dot) nsepseq reg (* contract_of M  *)
| E_Ctor     of ctor                           (* C              *)
| E_Div      of slash bin_op reg               (* x / y          *)
| E_DivEq    of div_eq bin_op reg              (* x /= y         *)
| E_Equal    of equal_cmp bin_op reg           (* x == y         *)
| E_Fun      of fun_expr reg                   (* (x: int) => x  *)
| E_Geq      of geq bin_op reg                 (* x >= y         *)
| E_Gt       of gt bin_op reg                  (* x > y          *)
| E_Int      of (lexeme * Z.t) wrap            (* 42             *)
| E_Leq      of leq bin_op reg                 (* x <= y         *)
| E_Lt       of lt bin_op reg                  (* x < y          *)
| E_MinusEq  of minus_eq bin_op reg            (* x -= y         *)
| E_Mod      of modulo bin_op reg              (* x % n          *)
| E_ModEq    of mod_eq bin_op reg              (* x %= y         *)
| E_ModPath  of expr module_path reg           (* M.N.x          *)
| E_Mult     of times bin_op reg               (* x * y          *)
| E_Neg      of minus un_op reg                (* -x             *)
| E_Neq      of neq bin_op reg                 (* x != y         *)
| E_Not      of negation un_2op reg            (* !x             *)
| E_Or       of bool_or bin_op reg             (* x || y         *)
| E_Record   of record_expr                    (* {x : e, y}     *)
| E_Par      of expr par                       (* (x + y)        *)
| E_PostDecr of decrement_expr reg             (* x--            *)
| E_PostIncr of increment_expr reg             (* x++            *)
| E_PreDecr  of decrement_expr reg             (* --x            *)
| E_PreIncr  of increment_expr reg             (* ++x            *)
| E_Proj     of projection reg                 (* e.x.1          *)
| E_String   of lexeme wrap                    (* "abcdef"       *)
| E_Sub      of minus bin_op reg               (* x - y          *)
| E_Ternary  of ternary reg                    (* x ? y : z      *)
| E_TimesEq  of times_eq bin_op reg            (* x *= y         *)
| E_Tuple    of expr tuple                     (* [x, ...y, z]   *)
| E_Typed    of annot_expr reg                 (* (e as t)       *)
| E_Unit     of the_unit reg                   (* ()             *)
| E_Update   of update_expr braces             (* {...x, y : z}  *)
| E_Var      of variable                       (* x              *)
| E_Verbatim of lexeme wrap                    (* {|foo|}        *)

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

and parameters = (pattern, comma) sep_of_term par




(* XXX *)

and update_expr = {
  ellipsis : ellipsis;
  record   : expr;
  comma    : comma;
  updates  : nsepseq
}

and switch = {
  kwd_switch : kwd_switch;
  lpar       : lpar;
  expr       : expr;
  rpar       : rpar;
  lbrace     : lbrace;
  cases      : case nseq;
  rbrace     : rbrace
}

and case =
  Case        of switch_case
| CaseDefault of switch_default_case

and switch_case = {
  kwd_case   : kwd_case;
  expr       : expr;
  colon      : colon;
  statements : statements option
}

and switch_default_case = {
  kwd_default : kwd_default;
  colon       : colon;
  statements  : statements option
}

and tuple_item_rest = {
  ellipsis : ellipsis;
  expr     : expr
}

and tuple_item =
  Expr_entry of expr
| Rest_entry of tuple_item_rest reg

and property2 = {
  name       : expr;
  colon      : colon;
  value      : expr;
  attributes : attribute list
}

and property_rest = {
  ellipsis : ellipsis;
  expr     : expr
}

and property =
  Punned_property of expr reg
| Property        of property2 reg
| Property_rest   of property_rest reg


type the_unit = lpar * rpar

and ternary = {
  condition : expr;
  qmark     : qmark;
  truthy    : expr;
  colon     : colon;
  falsy     : expr
}

and assignment_operator =
  Times_eq (* *= *)
| Div_eq
| Min_eq
| Plus_eq
| Mod_eq

and operator =
  Eq
| Assignment_operator of assignment_operator

and record_expr = (property, comma) nsepseq braces


and namespace_statement =
  kwd_namespace * module_name * intf_annotation option * statements braces reg * attributes

and intf_expr =
  IInterface of intf_body
| IPath      of (module_name, dot) nsepseq reg

and interface_entry =
  IType      of (attributes * kwd_type * variable * equal * type_expr) reg
| IType_var  of (attributes * kwd_type * variable) reg
| IConst     of (attributes * kwd_const * variable * colon * type_expr) reg

and interface_entries = (interface_entry, semi) nsepseq

and interface_body = interface_entries braces reg

and interface_annotation = (kwd_implements * interface_expr) reg

and interface_statement =
  kwd_interface * module_name * interface_body * attributes

and while_stmt = {
  kwd_while : kwd_while;
  lpar      : lpar;
  expr      : expr;
  rpar      : rpar;
  statement : statement
}

and for_of = {
  attributes : attributes;
  kwd_for    : kwd_for;
  lpar       : lpar;
  index_kind : val_kind;
  index      : variable;
  kwd_of     : kwd_of;
  expr       : expr;
  rpar       : rpar;
  statement  : statement
}

and for_stmt = {
  attributes   : attributes;
  kwd_for      : kwd_for;
  lpar         : lpar;
  initialiser  : statement option;
  semi1        : semi;
  condition    : expr option;
  semi2        : semi;
  afterthought : (expr, comma) nsepseq option;
  rpar         : rpar;
  statement    : statement option
}

and import =
  Import_rename   of import_rename
| Import_all_as   of import_all_as
| Import_selected of import_selected

and import_rename = {
  kwd_import  : kwd_import;
  alias       : module_name;
  equal       : equal;
  module_path : (module_name, dot) nsepseq
}

and import_all_as = {
  kwd_import  : kwd_import;
  times       : times;
  kwd_as      : kwd_as;
  alias       : module_name;
  kwd_from    : kwd_from;
  module_path : string wrap
}

and import_selected = {
  kwd_import  : kwd_import;
  imported    : (field_name, comma) nsepseq braces reg;
  kwd_from    : kwd_from;
  module_path : string wrap
}

and statements = (statement, semi) nsepseq


(* TODO: Used with colon too in Parser *)
and annot_expr = expr * kwd_as * type_expr

and 'a ne_injection = {
  compound    : compound option;
  ne_elements : ('a, semi) nsepseq;
  terminator  : semi option;
  attributes  : attributes
}

and compound =
  Braces   of lbrace * rbrace
| Brackets of lbracket * rbracket

(* Binary and unary arithmetic operators *)

and 'a bin_op = {arg1: expr; op: 'a; arg2: expr}
and 'a  un_op = {op: 'a; arg: expr}

and projection = {
  expr      : expr;
  selection : selection;
}

and selection =
  FieldName of selection_field_name reg
| Component of expr brackets reg

and selection_field_name = {
  dot   : dot;
  value : variable
}

and body =
  FunctionBody   of statements braces reg
| ExpressionBody of expr

and cond_statement = {
  attributes : attributes;
  kwd_if     : kwd_if;
  test       : expr par;
  ifso       : statement;
  ifnot      : (kwd_else * statement) option;
}

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : language;
  code     : expr;
}

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

let type_expr_to_region = function
  TProd   {inside = {region; _}; _}
| TSum    {region; _}
| TObject {region; _}
| TApp    {region; _}
| TFun    {region; _}
| TPar    {region; _} -> region
| TString t
| TVar    t -> t#region
| TModA   {region; _} -> region
| TInt    t -> t#region
| TDisc   reg -> nsepseq_to_region (fun a -> a.Region.region) reg
| TParameter {region; _} -> region

let pattern_to_region = function
  PRest {region;_ }    | PAssign {region ;_ }
| PVar {region ;_ }    | PDestruct {region ;_ }
| PObject {region ;_ } | PArray {region; _} -> region
| PConstr p -> p#region

let bool_expr_to_region = function
  Or {region;_} | And {region;_}
| Not {region;_} -> region

let comp_expr_to_region = function
  Lt {region;_} | Leq {region;_}
| Gt {region;_} | Geq {region;_}
| Neq {region;_} | Equal {region;_} -> region

let logic_expr_to_region = function
  BoolExpr e -> bool_expr_to_region e
| CompExpr e -> comp_expr_to_region e

let arith_expr_to_region = function
  Add {region;_} | Sub {region;_} | Mult {region;_}
| Div {region;_} | Mod {region;_} | Neg {region;_} -> region
| Int e -> e#region

let string_expr_to_region = function
  Verbatim w | String w -> w#region

let rec expr_to_region = function
  ELogic e -> logic_expr_to_region e
| EArith e -> arith_expr_to_region e
| EString e -> string_expr_to_region e
| EAssign (f, _, e) ->
    Region.cover (expr_to_region f) (expr_to_region e)
| EConstr {region; _}
| EAnnot {region;_ } | EFun {region;_}
| ECall {region;_}   | EProj {region; _}
| EUnit {region;_}   | EPar {region;_}
| ESeq {region; _}   | EObject {region; _} | EArray { region; _}
| ECodeInj {region; _} | EModA { region; _} | ETernary {region; _} -> region
| EBytes v -> v#region
| EVar v -> v#region
| EContract {region; _}
| EPrefix {region;_}
| EPostfix {region;_} -> region

let statement_to_region = function
  SBreak b -> b#region
| SExpr (_,e) -> expr_to_region e
| SBlock {region; _ }
| SCond {region; _}
| SReturn {region; _}
| SLet  {region; _}
| SConst {region; _}
| SSwitch {region; _}
| SType {region; _}
| SImport {region; _}
| SExport {region; _}
| SForOf {region; _}
| SWhile {region; _}
| SInterface {region; _}
| SNamespace {region; _}
| SFor {region;_} -> region

let selection_to_region = function
  FieldName f -> f.region
| Component c -> c.region

let body_to_region = function
  FunctionBody {region; _} -> region
| ExpressionBody s -> expr_to_region s

let property_to_region = function
  Punned_property {region; _}
| Property {region; _}
| Property_rest {region; _} -> region

let tuple_item_to_region = function
  Expr_entry e -> expr_to_region e
| Rest_entry {region; _} -> region

let top_statement_to_region = function
  TopLevel (statement, None) -> statement_to_region statement
| TopLevel (statement, Some semi) ->
    Region.cover (statement_to_region statement) semi#region
| Directive d -> Directive.to_region d
