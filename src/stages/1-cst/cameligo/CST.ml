(* Concrete Syntax Tree (CST) for CameLIGO *)

(* Disabled warnings *)

[@@@warning "-30"] (* multiply-defined record labels *)

(* Vendor dependencies *)

module Directive = Preprocessor.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

(* Local dependencies *)

module Wrap = Lexing_shared.Wrap
module Attr = Lexing_shared.Attr

(* Utilities *)

type 'a reg = 'a Region.reg
type 'payload wrap = 'payload Wrap.t

open Utils

type lexeme = string

(* Keywords of CameLIGO *)

(* IMPORTANT: The types are sorted alphabetically. If you add or
   modify some, please make sure they remain in order. *)

type kwd_and    = lexeme wrap
type kwd_begin  = lexeme wrap
type kwd_else   = lexeme wrap
type kwd_end    = lexeme wrap
type kwd_false  = lexeme wrap
type kwd_fun    = lexeme wrap
type kwd_if     = lexeme wrap
type kwd_in     = lexeme wrap
type kwd_let    = lexeme wrap
type kwd_land   = lexeme wrap
type kwd_lor    = lexeme wrap
type kwd_lxor   = lexeme wrap
type kwd_lsl    = lexeme wrap
type kwd_lsr    = lexeme wrap
type kwd_match  = lexeme wrap
type kwd_mod    = lexeme wrap
type kwd_module = lexeme wrap
type kwd_not    = lexeme wrap
type kwd_of     = lexeme wrap
type kwd_or     = lexeme wrap
type kwd_rec    = lexeme wrap
type kwd_struct = lexeme wrap
type kwd_then   = lexeme wrap
type kwd_true   = lexeme wrap
type kwd_type   = lexeme wrap
type kwd_with   = lexeme wrap

(* Symbols *)

(* IMPORTANT: The types are sorted alphabetically. If you add or
   modify some, please make sure they remain in order. *)

type arrow    = lexeme wrap  (* -> *)
type append   = lexeme wrap  (* @  *)
type bool_or  = lexeme wrap  (* || *)
type bool_and = lexeme wrap  (* && *)
type cat      = lexeme wrap  (* ^  *)
type colon    = lexeme wrap  (* :  *)
type comma    = lexeme wrap  (* ,  *)
type cons     = lexeme wrap  (* :: *)
type dot      = lexeme wrap  (* .  *)
type equal    = lexeme wrap  (* =  *)
type geq      = lexeme wrap  (* >= *)
type gt       = lexeme wrap  (* >  *)
type lbrace   = lexeme wrap  (* {  *)
type lbracket = lexeme wrap  (* [  *)
type leq      = lexeme wrap  (* =< *)
type lpar     = lexeme wrap  (* (  *)
type lt       = lexeme wrap  (* <  *)
type minus    = lexeme wrap  (* -  *)
type neq      = lexeme wrap  (* <> *)
type plus     = lexeme wrap  (* +  *)
type quote    = lexeme wrap  (* '  *)
type rbracket = lexeme wrap  (* ]  *)
type rbrace   = lexeme wrap  (* }  *)
type rev_app  = lexeme wrap  (* |> *)
type rpar     = lexeme wrap  (* )  *)
type semi     = lexeme wrap  (* ;  *)
type slash    = lexeme wrap  (* /  *)
type times    = lexeme wrap  (* *  *)
type vbar     = lexeme wrap  (* |  *)

(* End-of-File *)

type eof = lexeme wrap

(* Literals *)

type variable    = lexeme wrap
type module_name = lexeme wrap
type field_name  = lexeme wrap
type ctor        = lexeme wrap
type language    = lexeme reg   (* Not [wrap] *)
type attribute   = Attr.t reg

(* Parentheses *)

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

(* The Abstract Syntax Tree *)

type t = {
  decl : declaration nseq;
  eof  : eof
}

and cst = t

(* DECLARATIONS (top-level) *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and declaration =
  D_Attr      of (attribute * declaration) reg
| D_Directive of Directive.t
| D_Let       of let_decl reg
| D_Module    of module_decl reg
| D_Type      of type_decl reg

(* Non-recursive, top-level values *)

and let_decl = kwd_let * kwd_rec option * let_binding

and let_binding = {
  type_params : type_params par reg option;
  binders     : pattern nseq;
  rhs_type    : (colon * type_expr) option;
  eq          : equal;
  let_rhs     : expr
}

(* Type parameters *)

and type_params = {
  kwd_type  : kwd_type;
  type_vars : variable nseq
}

(* Module declaration *)

and module_decl = {
  kwd_module  : kwd_module;
  name        : module_name;
  eq          : equal;
  module_expr : module_expr
}

and module_expr =
  M_Body of module_body reg             (* Structure definition *)
| M_Path of module_name module_path reg (* Module selection     *)
| M_Var  of module_name                 (* Module aliasing      *)

and module_body = {
  kwd_struct   : kwd_struct;
  declarations : declarations;
  kwd_end      : kwd_end
}

(* Module paths *)

and 'a module_path = {
  module_path : (module_name, dot) nsepseq;
  selector    : dot;
  field       : 'a
}

(* Type declaration *)

and type_decl = {
  kwd_type  : kwd_type;
  name      : variable;
  params    : type_vars option;
  eq        : equal;
  type_expr : type_expr
}

and type_vars =
  TV_Single of type_var
| TV_Tuple  of type_var tuple

and type_var = (quote * variable) reg

and 'a tuple = ('a, comma) nsepseq par reg

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and type_expr =
  T_Arg     of type_var reg                        (*              'a *)
| T_App     of (type_expr * type_ctor_args) reg    (*     M.t (x,y,z) *)
| T_Attr    of (attribute * type_expr)             (*          [@a] x *)
| T_Cart    of cartesian                           (*     x * (y * z) *)
| T_Fun     of (type_expr * arrow * type_expr) reg (*          x -> y *)
| T_Int     of (lexeme * Z.t) wrap                 (*              42 *)
| T_ModPath of type_expr module_path reg           (*     A.B.(x * y) *)
| T_Par     of type_expr par reg                   (*             (t) *)
| T_Record  of field_decl reg record reg           (* {a; [@x] b : t} *)
| T_String  of lexeme wrap                         (*             "x" *)
| T_Variant of variant_type reg                    (* [@a] A | B of t *)
| T_Var     of variable                            (*               x *)

(* Type application *)

and type_ctor_args =
  TC_Single of type_expr
| TC_Tuple  of type_expr tuple

(* Cartesian type *)

and cartesian = (type_expr, times) nsepseq reg

(* Record type *)

and 'a record = {
  lbrace     : lbrace;
  elements   : ('a, semi) sepseq;
  terminator : semi option;
  rbrace     : rbrace
}

and field_decl = {
  field_name : field_name;
  field_type : type_annotation option; (* Type punning if [None] *)
  attributes : attribute list
}

and type_annotation = colon * type_expr

(* Variant type *)

and variant_type = {
  lead_vbar : vbar option;
  variants  : (variant reg, vbar) nsepseq
}

and variant = {
  ctor       : ctor;
  ctor_args  : (kwd_of * type_expr) option;
  attributes : attribute list
}

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and pattern =
  P_App      of (pattern * pattern option) reg      (*   M.C (x,y) *)
| P_Attr     of (attribute * pattern)               (*    [@var] x *)
| P_Bytes    of (lexeme * Hex.t) reg                (*      0xFFFA *)
| P_Cons     of (pattern * cons * pattern) reg      (*      x :: y *)
| P_Ctor     of ctor                                (*           C *)
| P_Int      of (lexeme * Z.t) reg                  (*          42 *)
| P_List     of pattern list_ reg                   (*      [x; 4] *)
| P_ModPath  of pattern module_path reg             (*       M.N.x *)
| P_Mutez    of (lexeme * Int64.t) wrap             (*      5mutez *)
| P_Nat      of (lexeme * Z.t) wrap                 (*          4n *)
| P_Par      of pattern par reg                     (*      (C, 4) *)
| P_Record   of field_pattern reg record reg        (*    {x=y; z} *)
| P_String   of lexeme wrap                         (*    "string" *)
| P_Tuple    of pattern tuple                       (*      (1, x) *)
| P_Typed    of typed_pattern reg                   (*   (x : int) *)
| P_Var      of variable                            (*           x *)
| P_Verbatim of lexeme wrap                         (*     {|foo|} *)
| P_Unit     of the_unit reg                        (*          () *)

(* List pattern *)

and 'a list_ = {
  lbracket   : lbracket;
  elements   : ('a, semi) sepseq;
  terminator : semi option;
  rbracket   : rbracket
}

(* Record pattern *)

and field_pattern = {
  field_name : field_name;
  eq         : equal;
  pattern    : pattern
}

(* Typed pattern *)

and typed_pattern = {
  pattern    : pattern;
  type_annot : type_annotation
}

(* Unit pattern *)

and the_unit = lpar * rpar

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and expr =
  E_Add      of plus bin_op reg              (* x + y               *)
| E_And      of kwd_and bin_op reg           (* x and y             *)
| E_App      of (expr * expr nseq) reg       (* f x y               *)
| E_Attr     of (attribute * expr)           (* [@a] (x,y)          *)
| E_Bytes    of (lexeme * Hex.t) wrap        (* 0xFFFA              *)
| E_Cat      of cat bin_op reg               (* "Hello" ^ world     *)
| E_CodeInj  of code_inj reg
| E_Cond     of cond_expr reg                (* if x then y         *)
| E_Ctor     of ctor                         (* C                   *)
| E_Cons     of cons bin_op reg              (* head :: tail        *)
| E_Div      of slash bin_op reg             (* x / y               *)
| E_Equal    of equal bin_op reg             (* x = y               *)
| E_Fun      of fun_expr reg                 (* fun x -> x          *)
| E_Geq      of geq bin_op reg               (* x >= y              *)
| E_Gt       of gt bin_op reg                (* x > y               *)
| E_Int      of (lexeme * Z.t) wrap          (* 42                  *)
| E_Land     of kwd_land bin_op reg          (* x land y            *)
| E_LetIn    of let_in reg
| E_Leq      of leq bin_op reg               (* x <= y              *)
| E_List     of expr list_ reg               (* [f x; 5]            *)
| E_Lor      of kwd_lor bin_op reg           (* x lor y             *)
| E_Lsl      of kwd_lsl bin_op reg           (* x lsl y             *)
| E_Lsr      of kwd_lsr bin_op reg           (* x lsr y             *)
| E_Lt       of lt bin_op reg                (* x < y               *)
| E_Lxor     of kwd_lxor bin_op reg          (* x lxor y            *)
| E_Match    of expr match_ reg              (* match e with p -> i *)
| E_Mod      of kwd_mod bin_op reg           (* x mod n             *)
| E_ModIn    of module_in reg                (* module M = N in e   *)
| E_ModPath  of expr module_path reg         (* M.N.x               *)
| E_Mult     of times bin_op reg             (* x * y               *)
| E_Mutez    of (lexeme * Int64.t) wrap      (* 5mutez              *)
| E_Nat      of (lexeme * Z.t) wrap          (* 4n                  *)
| E_Neg      of minus un_op reg              (* -a                  *)
| E_Neq      of neq bin_op reg               (* x =/= y             *)
| E_Not      of kwd_not un_op reg            (* not x               *)
| E_Or       of kwd_or bin_op reg            (* x or y              *)
| E_Par      of expr par reg                 (* (x - M.y)           *)
| E_Proj     of projection reg               (* e.x.1               *)
| E_Record   of field_assign reg record reg  (* {x=y; z}            *)
| E_String   of lexeme wrap                  (* "string"            *)
| E_Sub      of minus bin_op reg             (* a - b               *)
| E_Tuple    of expr tuple                   (* (1, x)              *)
| E_Typed    of typed_expr par reg           (* (x : int)           *)
| E_TypeIn   of type_in reg                  (* type t = u in e     *)
| E_Unit     of the_unit reg                 (* ()                  *)
| E_Update   of update reg                   (* {x with y=z}        *)
| E_Var      of variable                     (* x                   *)
| E_Verbatim of lexeme wrap                  (* {|foo|}             *)
| E_Seq      of sequence_expr reg            (* x; 3                *)
| E_RevApp   of rev_app bin_op reg           (* x |> f |> g y       *)

(* Typed expression *)

and typed_expr = expr * type_annotation

(* Sequence expression *)

and 'a sequence_expr = {
  compound   : compound option;
  elements   : (expr, semi) sepseq;
  terminator : semi option
}

and compound =
  BeginEnd of kwd_begin * kwd_end
| Braces   of lbrace * rbrace

(* Binary and unary arithmetic operators *)

and 'a bin_op = {
  op   : 'a;
  arg1 : expr;
  arg2 : expr
}

and 'a un_op = {
  op  : 'a;
  arg : expr
}

and 'a module_access = {
  module_name : module_name;
  selector    : dot;
  field       : 'a;
}

and projection = {
  struct_name : variable;
  selector    : dot;
  field_path  : (selection, dot) nsepseq
}

and selection =
  FieldName of variable
| Component of (string * Z.t) reg

and field_assign =
  Property of field_assign_property
| Punned_property of field_name

and field_assign_property = {
  field_name : field_name;
  assignment : equal;
  field_expr : expr
}

and update = {
  lbrace   : lbrace;
  record   : path;
  kwd_with : kwd_with;
  updates  : field_path_assignment reg ne_injection reg;
  rbrace   : rbrace
}

and field_path_assignment =
  Path_property of field_path_assignment_property
| Path_punned_property of field_name

and field_path_assignment_property =  {
  field_path : path;
  assignment : equal;
  field_expr : expr
}

and path =
  Name of variable
| Path of projection reg

and 'a match_ = {
  kwd_match : kwd_match;
  expr      : expr;
  kwd_with  : kwd_with;
  lead_vbar : vbar option;
  clauses   : ('a match_clause reg, vbar) nsepseq
}

and 'a match_clause = {
  pattern : pattern;
  arrow   : arrow;
  rhs     : 'a
}

and let_in = {
  kwd_let    : kwd_let;
  kwd_rec    : kwd_rec option;
  binding    : let_binding;
  kwd_in     : kwd_in;
  body       : expr;
  attributes : attribute list
}

and type_in = {
  type_decl  : type_decl;
  kwd_in     : kwd_in;
  body       : expr;
}

and module_in = {
  mod_decl : module_decl;
  kwd_in   : kwd_in;
  body     : expr;
}

and fun_expr = {
  kwd_fun     : kwd_fun;
  type_params : type_params par reg option;
  binders     : pattern nseq;
  rhs_type    : (colon * type_expr) option;
  arrow       : arrow;
  body        : expr;
  attributes  : attribute list
}

and cond_expr = {
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  ifso     : expr;
  ifnot    : (kwd_else * expr) option
}

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : language reg;
  code     : expr;
  rbracket : rbracket;
}

(* Projecting regions from some nodes of the AST *)

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nsepseq_to_region to_region (hd,tl) =
  let reg (_, item) = to_region item in
  Region.cover (to_region hd) (last reg tl)

let type_expr_to_region = function
  TProd   {region; _}
| TSum    {region; _}
| TRecord {region; _}
| TApp    {region; _}
| TFun    {region; _}
| TPar    {region; _}
| TString {region; _}
| TInt    {region; _}
| TVar    {region; _}
| TModA   {region; _}
| TArg    {region; _}
 -> region

let list_pattern_to_region = function
  PListComp {region; _} | PCons {region; _} -> region

let pattern_to_region = function
| PList p -> list_pattern_to_region p
| PConstr {region; _}
| PUnit {region;_}
| PTuple {region;_} | PVar {region;_}
| PInt {region;_}
| PString {region;_} | PVerbatim {region;_}
| PPar {region;_}
| PRecord {region; _} | PTyped {region; _}
| PNat {region; _} | PBytes {region; _}
  -> region

let bool_expr_to_region = function
  Or {region;_} | And {region;_} | Not {region;_} -> region

let comp_expr_to_region = function
  Lt {region;_} | Leq {region;_}
| Gt {region;_} | Geq {region;_}
| Neq {region;_} | Equal {region;_} -> region

let logic_expr_to_region = function
  BoolExpr e -> bool_expr_to_region e
| CompExpr e -> comp_expr_to_region e

let arith_expr_to_region = function
  Add {region;_} | Sub {region;_} | Mult {region;_}
| Div {region;_} | Mod {region;_} | Land {region;_}
| Lor {region;_} | Lxor {region;_} | Lsl {region;_} | Lsr {region;_}
| Neg {region;_} | Int {region;_} | Mutez {region; _}
| Nat {region; _} -> region

let string_expr_to_region = function
  Verbatim {region;_} | String {region;_} | Cat {region;_} -> region

let list_expr_to_region = function
  ECons {region; _} | EListComp {region; _} -> region

let expr_to_region = function
  ELogic e -> logic_expr_to_region e
| EArith e -> arith_expr_to_region e
| EString e -> string_expr_to_region e
| EList e -> list_expr_to_region e
| EConstr {region; _}
| EAnnot {region;_ } | ELetIn {region;_}   | EFun {region;_}
| ETypeIn {region;_ }| EModIn {region;_}   | EModAlias {region;_}
| ECond {region;_}   | ETuple {region;_}   | ECase {region;_}
| ECall {region;_}   | EVar {region; _}    | EProj {region; _}
| EUnit {region;_}   | EPar {region;_}     | EBytes {region; _}
| ESeq {region; _}   | ERecord {region; _} | EUpdate {region; _}
| EModA {region; _} | ECodeInj {region; _}
| ERevApp {region; _} -> region

let selection_to_region = function
  FieldName f -> f.region
| Component c -> c.region

let path_to_region = function
  Name var -> var.region
| Path {region; _} -> region

let type_ctor_arg_to_region = function
  CArg  t -> type_expr_to_region t
| CArgTuple t -> t.region
