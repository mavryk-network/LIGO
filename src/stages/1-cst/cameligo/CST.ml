(* Concrete Syntax Tree (CST) for CameLIGO *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-30-40-42"]
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

(* Lexemes *)

type lexeme = string [@@deriving yojson]
let wrap_to_yojson = Wrap.to_yojson
let wrap_of_yojson = Wrap.of_yojson
let reg_to_yojson = Region.reg_to_yojson
let reg_of_yojson = Region.reg_of_yojson

(* Keywords of CameLIGO *)

type keyword       = lexeme wrap [@@deriving yojson]
type kwd_and       = lexeme wrap [@@deriving yojson]
type kwd_begin     = lexeme wrap [@@deriving yojson]
type kwd_else      = lexeme wrap [@@deriving yojson]
type kwd_end       = lexeme wrap [@@deriving yojson]
type kwd_false     = lexeme wrap [@@deriving yojson]
type kwd_fun       = lexeme wrap [@@deriving yojson]
type kwd_rec       = lexeme wrap [@@deriving yojson]
type kwd_if        = lexeme wrap [@@deriving yojson]
type kwd_in        = lexeme wrap [@@deriving yojson]
type kwd_let       = lexeme wrap [@@deriving yojson]
type kwd_match     = lexeme wrap [@@deriving yojson]
type kwd_mod       = lexeme wrap [@@deriving yojson]
type kwd_land      = lexeme wrap [@@deriving yojson]
type kwd_lor       = lexeme wrap [@@deriving yojson]
type kwd_lxor      = lexeme wrap [@@deriving yojson]
type kwd_lsl       = lexeme wrap [@@deriving yojson]
type kwd_lsr       = lexeme wrap [@@deriving yojson]
type kwd_not       = lexeme wrap [@@deriving yojson]
type kwd_of        = lexeme wrap [@@deriving yojson]
type kwd_or        = lexeme wrap [@@deriving yojson]
type kwd_then      = lexeme wrap [@@deriving yojson]
type kwd_true      = lexeme wrap [@@deriving yojson]
type kwd_type      = lexeme wrap [@@deriving yojson]
type kwd_with      = lexeme wrap [@@deriving yojson]
type kwd_module    = lexeme wrap [@@deriving yojson]
type kwd_struct    = lexeme wrap [@@deriving yojson]

(* Symbols *)

type arrow    = lexeme wrap [@@deriving yojson]  (* "->" *)
type cons     = lexeme wrap [@@deriving yojson]  (* "::" *)
type cat      = lexeme wrap [@@deriving yojson]  (* "^"  *)
type append   = lexeme wrap [@@deriving yojson]  (* "@"  *)
type dot      = lexeme wrap [@@deriving yojson]  (* "."  *)

(* Arithmetic operators *)

type minus    = lexeme wrap [@@deriving yojson]  (* "-" *)
type plus     = lexeme wrap [@@deriving yojson]  (* "+" *)
type slash    = lexeme wrap [@@deriving yojson]  (* "/" *)
type times    = lexeme wrap [@@deriving yojson]  (* "*" *)

(* Boolean operators *)

type bool_or  = lexeme wrap [@@deriving yojson]  (* "||" *)
type bool_and = lexeme wrap [@@deriving yojson]  (* "&&" *)

(* Reverse application *)

type rev_app = lexeme wrap [@@deriving yojson] (* "|>" *)

(* Comparisons *)

type equal = lexeme wrap [@@deriving yojson]  (* "="  *)
type neq   = lexeme wrap [@@deriving yojson]  (* "<>" *)
type lt    = lexeme wrap [@@deriving yojson]  (* "<"  *)
type gt    = lexeme wrap [@@deriving yojson]  (* ">"  *)
type leq   = lexeme wrap [@@deriving yojson]  (* "=<" *)
type geq   = lexeme wrap [@@deriving yojson]  (* ">=" *)

(* Compounds *)

type lpar     = lexeme wrap [@@deriving yojson]  (* "(" *)
type rpar     = lexeme wrap [@@deriving yojson]  (* ")" *)
type lbracket = lexeme wrap [@@deriving yojson]  (* "[" *)
type rbracket = lexeme wrap [@@deriving yojson]  (* "]" *)
type lbrace   = lexeme wrap [@@deriving yojson]  (* "{" *)
type rbrace   = lexeme wrap [@@deriving yojson]  (* "}" *)

(* Separators *)

type comma = lexeme wrap [@@deriving yojson]  (* "," *)
type semi  = lexeme wrap [@@deriving yojson]  (* ";" *)
type vbar  = lexeme wrap [@@deriving yojson]  (* "|" *)
type colon = lexeme wrap [@@deriving yojson]  (* ":" *)

type quote = lexeme wrap [@@deriving yojson]  (* "'" *)

(* Wildcard *)

type wild = lexeme wrap [@@deriving yojson]  (* "_" *)

(* Virtual tokens *)

type eof = lexeme wrap [@@deriving yojson]

(* Literals *)

type variable    = string reg [@@deriving yojson]
type module_name = string reg [@@deriving yojson]
type fun_name    = string reg [@@deriving yojson]
type type_name   = string reg [@@deriving yojson]
type field_name  = string reg [@@deriving yojson]
type type_constr = string reg [@@deriving yojson]
type constr      = string reg [@@deriving yojson]
type type_param  = string reg [@@deriving yojson]

type attribute   = Attr.t [@@deriving yojson]
type attributes  = Attr.attribute reg list [@@deriving yojson]

(* Parentheses *)

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
} [@@deriving yojson]

type the_unit = lpar * rpar

let nsepseq_to_yojson value_to_yojson sep_to_yojson (hd, sep, tl) =
  `List [value_to_yojson hd; `List (List.map ~f:(fun x -> (`List [sep_to_yojson sep; value_to_yojson x])) tl)]

let nsepseq_of_yojson value_of_yojson sep_of_yojson (yojson: Yojson.Safe.t) =
  match yojson with
    `List [hd; `List tl] ->
     let yojsons = List.map ~f:(fun x ->
                       match x with
                       | `List [sep; tl] -> (match sep_of_yojson sep, value_of_yojson tl with
                                             | Ok sep, Ok tl -> Ok (sep, tl)
                                             | _ -> Error "nsepseq_of_yojson failed")  
                       | _ -> Error "nsepseq_of_yojson failed") tl in
    let tl = List.fold_right ~f:(fun v acc -> match v, acc with
                                              | Ok (sep, v), Ok l -> Ok ((sep, v) :: l)
                                              | _, (Error _ as e) -> e 
                                              | Error _ as e, _ -> e) yojsons ~init: (Ok []) in
    (match value_of_yojson hd, tl with
     | Ok hd, Ok tl -> Ok (hd, tl)
     | _ -> Error "nseq_of_yojson failed")
  | _ -> Error "nseq_of_yojson failed"

let nseq_to_yojson value_to_yojson (hd, tl) =
  `List [value_to_yojson hd; `List (List.map ~f:value_to_yojson tl)]

let nseq_of_yojson value_of_yojson (yojson: Yojson.Safe.t) =
  match yojson with
    `List [hd; `List tl] ->
    let yojsons = List.map ~f:value_of_yojson tl in
    let tl = List.fold_right ~f:(fun v acc -> match v, acc with
                                              | Ok v, Ok l -> Ok (v :: l)
                                              | _, (Error _ as e) -> e 
                                              | Error _ as e, _ -> e) yojsons ~init: (Ok []) in
    (match value_of_yojson hd, tl with
     | Ok hd, Ok tl -> Ok (hd, tl)
     | _ -> Error "nseq_of_yojson failed")
  | _ -> Error "nseq_of_yojson failed"

(* The Abstract Syntax Tree *)

type t = {
  decl : declaration nseq;
  eof  : eof
} [@@deriving yojson]


and ast = t

and declaration =
  Let         of let_decl     reg
| TypeDecl    of type_decl    reg
| ModuleDecl  of module_decl  reg
| ModuleAlias of module_alias reg
| Directive   of Directive.t [@@deriving yojson]


(* Non-recursive values *)

and let_decl =
  (kwd_let * kwd_rec option * let_binding * attributes) [@@deriving yojson]

and let_binding = {
  type_params : type_params par reg option;
  binders     : pattern nseq;
  rhs_type    : (colon * type_expr) option;
  eq          : equal;
  let_rhs     : expr
} [@@deriving yojson]

and type_params = {
  kwd_type  : kwd_type;
  type_vars : variable nseq
} [@@deriving yojson]

(* Type declarations *)

and type_decl = {
  kwd_type   : kwd_type;
  name       : type_name;
  params     : type_vars option;
  eq         : equal;
  type_expr  : type_expr
} [@@deriving yojson]

and type_vars =
  QParam      of type_var reg
| QParamTuple of (type_var reg, comma) nsepseq par reg [@@deriving yojson]

and type_var = {
  quote : quote;
  name  : variable
} [@@deriving yojson]

and module_decl = {
  kwd_module : kwd_module;
  name       : module_name;
  eq         : equal;
  kwd_struct : kwd_struct;
  module_    : t;
  kwd_end    : kwd_end;
} [@@deriving yojson]

and module_alias = {
  kwd_module : kwd_module;
  alias      : module_name;
  eq         : equal;
  binders    : (module_name, dot) nsepseq;
} [@@deriving yojson]

and type_expr =
  TProd   of cartesian
| TSum    of sum_type reg
| TRecord of field_decl reg ne_injection reg
| TApp    of (type_constr * type_constr_arg) reg
| TFun    of (type_expr * arrow * type_expr) reg
| TPar    of type_expr par reg
| TVar    of variable
| TString of lexeme reg
| TInt    of (lexeme * Z.t) reg
| TModA   of type_expr module_access reg
| TArg    of type_var reg [@@deriving yojson]

and type_constr_arg =
  CArg      of type_expr
| CArgTuple of (type_expr, comma) nsepseq par reg [@@deriving yojson]

and cartesian = (type_expr, times) nsepseq reg [@@deriving yojson]

and sum_type = {
  lead_vbar  : vbar option;
  variants   : (variant reg, vbar) nsepseq;
  attributes : attributes
} [@@deriving yojson]

and variant = {
  constr     : constr;
  arg        : (kwd_of * type_expr) option;
  attributes : attributes
} [@@deriving yojson]

and field_decl = {
  field_name : field_name;
  colon      : colon;
  field_type : type_expr;
  attributes : attributes
} [@@deriving yojson]

and pattern =
  PConstr   of (constr * pattern option) reg
| PUnit     of the_unit reg
| PVar      of var_pattern reg
| PInt      of (lexeme * Z.t) reg
| PNat      of (lexeme * Z.t) reg
| PBytes    of (lexeme * Hex.t) reg
| PString   of string reg
| PVerbatim of string reg
| PList     of list_pattern
| PTuple    of (pattern, comma) nsepseq reg
| PPar      of pattern par reg
| PRecord   of field_pattern reg ne_injection reg
| PTyped    of typed_pattern reg [@@deriving yojson]

and var_pattern = {
  variable   : variable;
  attributes : attributes
} [@@deriving yojson]

and list_pattern =
  PListComp of pattern injection reg
| PCons     of (pattern * cons * pattern) reg [@@deriving yojson]

and typed_pattern = {
  pattern   : pattern;
  colon     : colon;
  type_expr : type_expr
}

and field_pattern = {
  field_name : field_name;
  eq         : equal;
  pattern    : pattern
}

and expr =
  ECase     of expr case reg
| ECond     of cond_expr reg
| EAnnot    of annot_expr par reg
| ELogic    of logic_expr
| EArith    of arith_expr
| EString   of string_expr
| EList     of list_expr
| EConstr   of (constr * expr option) reg
| ERecord   of record reg
| EProj     of projection reg
| EModA     of expr module_access reg
| EUpdate   of update reg
| EVar      of variable
| ECall     of (expr * expr nseq) reg
| EBytes    of (string * Hex.t) reg
| EUnit     of the_unit reg
| ETuple    of (expr, comma) nsepseq reg
| EPar      of expr par reg
| ELetIn    of let_in reg
| ETypeIn   of type_in reg
| EModIn    of mod_in reg
| EModAlias of mod_alias reg
| EFun      of fun_expr reg
| ESeq      of expr injection reg
| ECodeInj  of code_inj reg
| ERevApp   of rev_app bin_op reg

and annot_expr = expr * colon * type_expr

and 'a injection = {
  compound   : compound option;
  elements   : ('a, semi) sepseq;
  terminator : semi option
}

and 'a ne_injection = {
  compound    : compound option;
  ne_elements : ('a, semi) nsepseq;
  terminator  : semi option;
  attributes  : attributes
}

and compound =
  BeginEnd of kwd_begin * kwd_end
| Braces   of lbrace * rbrace
| Brackets of lbracket * rbracket

and list_expr =
  ECons     of cons bin_op reg
| EListComp of expr injection reg
  (*| Append of (expr * append * expr) reg*)

and string_expr =
  Cat      of cat bin_op reg
| String   of string reg
| Verbatim of string reg

and arith_expr =
  Add   of plus bin_op reg
| Sub   of minus bin_op reg
| Mult  of times bin_op reg
| Div   of slash bin_op reg
| Mod   of kwd_mod bin_op reg
| Land  of kwd_land bin_op reg
| Lor   of kwd_lor bin_op reg
| Lxor  of kwd_lxor bin_op reg
| Lsl   of kwd_lsl bin_op reg
| Lsr   of kwd_lsr bin_op reg
| Neg   of minus un_op reg
| Int   of (string * Z.t) reg
| Nat   of (string * Z.t) reg
| Mutez of (string * Int64.t) reg

and logic_expr =
  BoolExpr of bool_expr
| CompExpr of comp_expr

and bool_expr =
  Or    of kwd_or bin_op reg
| And   of kwd_and bin_op reg
| Not   of kwd_not un_op reg

and 'a bin_op = {
  op   : 'a;
  arg1 : expr;
  arg2 : expr
}

and 'a un_op = {
  op  : 'a;
  arg : expr
}

and comp_expr =
  Lt    of lt    bin_op reg
| Leq   of leq   bin_op reg
| Gt    of gt    bin_op reg
| Geq   of geq   bin_op reg
| Equal of equal bin_op reg
| Neq   of neq   bin_op reg

and record = field_assign reg ne_injection

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

and 'a case = {
  kwd_match : kwd_match;
  expr      : expr;
  kwd_with  : kwd_with;
  lead_vbar : vbar option;
  cases     : ('a case_clause reg, vbar) nsepseq reg
}

and 'a case_clause = {
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
  attributes : attributes
}

and type_in = {
  type_decl  : type_decl;
  kwd_in     : kwd_in;
  body       : expr;
}

and mod_in = {
  mod_decl : module_decl;
  kwd_in   : kwd_in;
  body     : expr;
}

and mod_alias = {
  mod_alias : module_alias;
  kwd_in    : kwd_in;
  body      : expr;
}

and fun_expr = {
  kwd_fun     : kwd_fun;
  type_params : type_params par reg option;
  binders     : pattern nseq;
  rhs_type    : (colon * type_expr) option;
  arrow       : arrow;
  body        : expr;
  attributes  : attributes
}

and cond_expr = {
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  ifso     : expr;
  ifnot    : (kwd_else * expr) option;
}

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : string reg reg;
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

let lpar_to_yojson = Wrap.to_yojson

