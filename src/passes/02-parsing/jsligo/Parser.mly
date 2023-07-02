%{
(* START HEADER *)

[@@@warning "-42"]

(* Vendors dependencies *)

open Simple_utils.Region

(* LIGO dependencies *)

module CST = Cst_jsligo.CST
open! CST
module Wrap = Lexing_shared.Wrap
module Nodes = Cst_shared.Nodes

(* UTILITIES

   The following functions help build CST nodes. When they are
   complicated, like [mk_mod_path], it is because the grammar rule had
   to be written in a certain way to remain LR, and that way did not
   make it easy in the semantic action to collate the information into
   CST nodes. *)

let mk_reg region value = Region.{region; value}

let mk_mod_path :
  (module_name * dot) Utils.nseq * 'a ->
  ('a -> Region.t) ->
  'a CST.module_path Region.reg =
  fun (nseq, field) to_region ->
    let (first, sep), tail = nseq in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item, next_sep) :: others ->
        trans ((prev_sep, item) :: seq, next_sep) others in
    let list, last_dot = trans ([], sep) tail in
    let module_path = first, List.rev list in
    let region = CST.nseq_to_region (fun (x,_) -> x#region) nseq in
    let region = Region.cover region (to_region field)
    and value = {module_path; selector=last_dot; field}
    in {value; region}

let ghost = Wrap.ghost

let mk_wild_pattern variable =
  let region   = variable#region in
  let variable = Wrap.make "_" region in
  let value    = {variable; attributes=[]}
  in {region; value}

let last = Nodes.last
let nseq_to_region = Nodes.nseq_to_region
let nsepseq_to_region = Nodes.nsepseq_to_region

(* Hooking attributes, if any *)

let rec hook mk_attr attrs node =
  match attrs with
    []            -> node
  | attr :: attrs -> mk_attr attr @@ hook mk_attr attrs node

let hook_E_Attr = hook @@ fun a e -> E_Attr (a, e)
let hook_T_Attr = hook @@ fun a t -> T_Attr (a, t)
let hook_S_Attr = hook @@ fun a s -> S_Attr (a, s)

(* END HEADER *)
%}

(* %attribute nsepseq(case_statement,SEMI) [@recover.cost 1004] *)
(* %attribute nsepseq(statement,SEMI)      [@recover.cost 1004] *)

(* Reductions on error *)

(* %on_error_reduce gt *)
(* %on_error_reduce nseq(Attr) *)
(* %on_error_reduce bin_op(add_expr_level,PLUS,mult_expr_level) *)
(* %on_error_reduce bin_op(add_expr_level,MINUS,mult_expr_level) *)
(* %on_error_reduce call_expr_level *)
(* %on_error_reduce bin_op(disj_expr_level,BOOL_OR,conj_expr_level) *)
(* %on_error_reduce type_expr *)
(* %on_error_reduce core_type *)
(* %on_error_reduce chevrons(type_ctor_args) *)
(* %on_error_reduce disj_expr_level *)
(* %on_error_reduce member_expr *)
(* %on_error_reduce add_expr_level *)
(* %on_error_reduce nsepseq(binding_initializer,COMMA) *)
(* %on_error_reduce nsepseq(module_name,DOT) *)
(* %on_error_reduce base_stmt(statement) *)
(* %on_error_reduce unary_expr_level *)
(* %on_error_reduce bin_op(comp_expr_level,NE,add_expr_level) *)
(* %on_error_reduce bin_op(comp_expr_level,LT,add_expr_level) *)
(* %on_error_reduce bin_op(comp_expr_level,LE,add_expr_level) *)
(* %on_error_reduce bin_op(comp_expr_level,gt,add_expr_level) *)
(* %on_error_reduce bin_op(comp_expr_level,ge,add_expr_level) *)
(* %on_error_reduce bin_op(comp_expr_level,EQ2,add_expr_level) *)
(* %on_error_reduce expr_stmt *)
(* %on_error_reduce expr *)
(* %on_error_reduce comp_expr_level *)
(* %on_error_reduce conj_expr_level *)
(* %on_error_reduce bin_op(conj_expr_level,BOOL_AND,comp_expr_level) *)
(* %on_error_reduce return_stmt *)
(* %on_error_reduce nsepseq(statement,SEMI) *)
(* %on_error_reduce nsepseq(variant,VBAR) *)
(* %on_error_reduce nsepseq(object_type,VBAR) *)
(* %on_error_reduce nsepseq(field_name,COMMA) *)
(* %on_error_reduce module_var_t *)
(* %on_error_reduce for_stmt(statement) *)
(* %on_error_reduce chevrons(nsepseq(type_var,COMMA)) *)

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <CST.t> contract
%type <CST.expr> interactive_expr

%%

(* RULES *)

(* Zero-Width SPace virtual token in context *)

gt:
  ">" ioption(ZWSP) { $1 }

ge:
  ">" ZWSP "=" { Wrap.wrap ">=" (cover $1#region $3#region) }

(* Compound constructs *)

par(X):
  "(" X ")" {
    let region = cover $1#region $3#region
    and value  = {lpar=$1; inside=$2; rpar=$3}
    in {region; value} }

chevrons(X):
  "<" X ">" ioption(ZWSP) {
    let region = cover $1#region $3#region
    and value  = {lchevron=$1; inside=$2; rchevron=$3}
    in {region; value} }

brackets(X):
  "[" X "]" {
    let region = cover $1#region $3#region
    and value  = {lbracket=$1; inside=$2; rbracket=$3}
    in {region; value} }

braces(X):
  "{" X "}" {
    let region = cover $1#region $3#region
    and value  = {lbrace=$1; inside=$2; rbrace=$3}
    in {region; value} }

(* Sequences

   Series of instances of the same syntactical category have often to
   be parsed, like lists of expressions, patterns etc. The simplest of
   all is the possibly empty sequence (series), parsed below by
   [seq]. The non-empty sequence is parsed by [nseq]. Note that the
   latter returns a pair made of the first parsed item (the parameter
   [X]) and the rest of the sequence (possibly empty). This way, the
   OCaml typechecker can keep track of this information along the
   static control-flow graph. See module [Utils] for the types
   corresponding to the semantic actions of those rules. *)

(* Non-empty sequence of items *)

nseq(X):
  X         { $1, [] }
| X nseq(X) { let hd,tl = $2 in $1, hd::tl }

(* Non-empty separated sequence of items *)

nsepseq(item,sep):
  item                       {                        $1, [] }
| item sep nsepseq(item,sep) { let h,t = $3 in $1, ($2,h)::t }

(* The rule [nsep_or_term(item,sep)] ("non-empty separated or
   terminated list") parses a non-empty list of items separated by
   [sep], and optionally terminated by [sep]. *)

nsep_or_term(item,sep):
  nsepseq(item,sep)      { `Sep  $1 }
| nseq(item sep {$1,$2}) { `Term $1 }

(* The rule [sep_or_term(item,sep)] ("separated or terminated list")
   parses a list of items separated by [sep], and optionally
   terminated by [sep]. *)

sep_or_term(item,sep):
  ioption(nsep_or_term(item,sep)) { $1 }

(* The rule [nsep_or_pref(item,sep)] ("non-empty separated or prefixed
   list") parses a non-empty list of items separated by [sep], and
   optionally prefixed by [sep]. *)

nsep_or_pref(item,sep):
  nsepseq(item,sep)      { `Sep  $1 }
| nseq(sep item {$1,$2}) { `Pref $1 }

(* Helpers *)

%inline
variable    : "<ident>"  { $1 }

type_var    : "<ident>" | "<uident>" { $1 }
type_name   : "<ident>" | "<uident>" { $1 }
type_ctor   : "<ident>" | "<uident>" { $1 }
field_name  : "<ident>"  { $1 }
module_name : "<uident>" { $1 }
intf_name   : "<uident>" { $1 }
ctor        : "<uident>" { $1 }
file_path   : "<string>" { $1 }

%inline
lang_name   : "<ident>" | "<uident>" { $1 }

(* Attributes *)

%inline
attributes:
  ioption(nseq("[@attr]") { Utils.nseq_to_list $1 }) {
    Option.value ~default:[] $1 }

(* ENTRY POINTS *)

interactive_expr: expr EOF { $1 }

contract:
  nseq(top_decl) EOF { {decl=$1; eof=$2} }

(* TOP-LEVEL DECLARATIONS *)

top_decl:
  declaration ";"?   { TL_Decl      ($1, $2) }
| "[@attr]" top_decl { TL_Attr      ($1, $2) }
| "<directive>"      { TL_Directive $1       }
| "export" top_decl  {
     let region = cover $1#region (top_decl_to_region $2)
     in TL_Export {region; value=($1,$2)} }

(* INNER DECLARATIONS (AS STATEMENTS) *)

declaration:
  value_decl     { D_Value     $1 }
| import_decl    { D_Import    $1 }
| interface_decl { D_Interface $1 }
| module_decl    { D_Module    $1 }
| type_decl      { D_Type      $1 }

(* Value declaration (constant and mutable) *)

value_decl:
  var_kind bindings {
    let start  = var_kind_to_region $1
    and stop   = nsepseq_to_region (fun x -> x.region) $2 in
    let region = cover start stop
    and value  = {kind=$1; bindings=$2}
    in {region; value} }

var_kind:
  "let"   { `Let   $1 }
| "const" { `Const $1 |

bindings:
  nsepseq(val_binding,",") { $1 }

val_binding:
  pattern ioption(type_vars) ioption(type_annotation) "=" expr {
    let region = cover (pattern_to_region $1) (expr_to_region $5)
    and value  = {pattern=$1; type_vars=$2; rhs_type=$3; eq=$4; rhs_expr=$5}
    in {region; value}

type_vars:
  chevrons(sep_or_term(type_var,",")) { $1 }

type_annotation:
  ":" type_expr { $1, $2 }

(* Import declaration *)

import_decl:
  "import" module_name "=" module_selection {
    let stop   = nsepseq_to_region (fun a -> a#region) $4 in
    let region = cover $1#region stop
    and value  = AliasModule {kwd_import=$1; alias=$2;
                              equal=$3; module_path=$4}
    in {region; value}
  }
| "import" "*" "as" module_name "from" file_path {
    let region = cover $1#region $6#region in
    let value  = ImportAll {kwd_import=$1; times=$2; kwd_as=$3; alias=$4;
                            kwd_from=$5; file_path=$6}
    in {region; value}
  }
| "import" braces(sep_or_term(field_name, ",")) "from" file_path {
    let region = cover $1#region $4#region in
    let value  = ImportSome {kwd_import=$1; imported=$2; kwd_from=$3;
                             file_path=$4}
    in {region; value} }

module_selection:
  module_path(module_name) { mk_mod_path $1 (fun w -> w#region) }

module_path(selected):
  module_name "." module_path(selected) {
    let (head, tail), selected = $3 in
    (($1,$2), head::tail), selected
  }
| module_name "." selected { (($1,$2), []), $3 }

(* Interface declaration *)

interface_decl:
  "interface" intf_name intf_body {
    let region = cover $1#region $3.region
    and value  = {kwd_interface=$1; intf_name=$2; intf_body=$3}
    in {region; value} }

intf_body:
  braces(intf_entries) { $1 }

intf_entries:
  sep_or_term(intf_entry,";") { $1 }

intf_entry:
  "[@attr]" intf_entry { I_Attr  ($1,$2) }
| intf_type            { I_Type  $1      }
| intf_const           { I_Const $1      }

intf_type:
  "type" type_name "=" type_expr {
    let value  = {kwd_type=$1; type_name=$2; type_rhs = Some $3}
    and stop   = type_expr_to_region $4 in
    let region = cover $1#region stop
    in {region; value}
  }
| "type" type_name {
    let value  = {kwd_type=$1; type_name=$2; type_rhs=None}
    and region = cover $1#region $2#region
    in {region; value} }

intf_const:
  "const" variable type_annotation {
    let _, t   = $3 in
    let stop   = type_expr_to_region in
    let region = cover $1#region (type_expr_to_region t)
    and value  = {kwd_const=$1; const_name=$2; const_type=$3}
    in {region; value} }

(* Module declaration *)

module_decl:
  "namespace" module_binder ioption(interface) braces(statements) {
     let region = cover $1#region $4.region
     and value  = {kwd_namespace=$1; module_name=$2; module_type=$3;
                   module_body=$4}
     in {region; value} }

module_binder:
  module_name | "_" { $1 }

interface:
  "implements" intf_expr {
     let region = cover $1#region (intf_expr_to_region $2)
     in {region; value=($1,$2)} }

intf_expr:
  intf_body        { I_Body $1 }
| module_selection { I_Path $1 }

(* Type declaration *)

type_decl:
  "type" type_binder ioption(type_vars) "=" type_expr {
    let region = cover $1#region (type_expr_to_region $5)
    and value  = {kwd_type=$1; name=$2; type_vars=$3; eq=$4; type_expr=$5}
    in {region; value}

type_binder:
  type_name | "_" { $1 }

type_vars:
  chevrons(nsepseq(type_var,",")) { $1 }

(* TYPE EXPRESSIONS *)

type_expr:
  fun_type | variant_type | core_type { $1 }

(* Functional types *)

fun_type:
  par(sep_or_term(fun_type_param,",")) "=>" type_expr {
    let stop   = type_expr_to_region $3 in
    let region = cover $1.region stop
    in T_Fun {region; value=($1,$2,$3)} }

fun_type_param:
  "<ident>" type_annotation {
    let region = cover $1#region (type_expr_to_region (snd $2))
    in {region; value = ($1,$2)} }

(* Variant types *)

variant_type:
  attributes nseq("|",variant) {
   (* Attributes of the variant type *)
    let region    = nseq_to_region (fun x -> x.region) $2 in
    let type_expr = T_Variant {region; value = `Pref $2}
    in hook_T_Attr $1 type_expr
  }
| nsepseq(variant, "|") {
    let region = nsepseq_to_region (fun x -> x.region) $1
    in T_Variant {region; value = `Sep $1} }

variant:
  attributes brackets(variant_comp) {
    let value = {attributes=$1; tuple=$2}
    in {region = $2.region; value} }

%inline
variant_comp:
  "<string>"                 { {ctor=$1; ctor_params = None}         }
| "<string>" "," ctor_params { {ctor=$1; ctor_params = Some ($2,$3)} }

ctor_params:
  nsep_or_term(ctor_param,",") { $1 }

ctor_param:
  type_expr { $1 }

(* Core types *)

(* The production [core_type_no_string] is here to avoid a conflict
   with a variant for a constant constructor, e.g. [["C"]], which
   could be interpreted otherwise as an type tuple (array) of the type
   ["C"]. *)

core_type:
  "<string>"            { T_String $1 }
| core_type_no_string   {          $1 }

core_type_no_string:
  "[@attr]" core_type_no_string { T_Attr ($1,$2) }
| no_attr_type                  { $1             }

%inline (* Was on [core_type_no_string] *)
no_attr_type:
  "<int>"           { T_Int       $1 }
| type_name         { T_Var       $1 } (* "_" unsupported in type checker *)
| type_ctor_app     { T_App       $1 }
| type_tuple        { T_Cart      $1 }
| par(type_expr)    { T_Par       $1 }
| parameter_of_type { T_Parameter $1 }
| qualified_type
| union_or_record   {             $1 }

(* Application of type arguments to type constructors *)

type_ctor_app:
  type_ctor type_ctor_args {
    let region = cover $1#region $2.region
    in {region; value = ($1,$2)} }

type_ctor_args:
  chevrons(nsep_or_term(type_ctor_arg,",")) { $1 }

type_ctor_arg:
  type_expr { $1 }

(* Tuples of types *)

type_tuple:
  brackets(type_components) { $1 }

type_components:
  type_component_no_string {
    `Sep ($1,[])
  }
| type_component_no_string "," nsep_or_term(type_component,",") {
    match $3 with
      `Sep  seq -> `Sep (Utils.nsepseq_cons $1 $2 seq)
    | `Term seq -> `Term (($1,$2) :: seq) }

type_component_no_string:
  fun_type | sum_type | core_type_no_string { $1 }

type_component:
  type_expr { $1 }

(* Record types (a.k.a. "object types" in JS) *)

record(field_kind):
  braces(sep_of_term(field(field_kind),object_sep)) { $1 }

field(field_kind):
  attributes field_name ioption(":" field_kind { $1,$2 }) {
    fun region_of_field_kind ->
      let region =
        match $3 with
          None -> $2#region
        | Some (_, k) -> cover $2#region (region_of_field_kind k)
      and value = {attributes=$1; field_name=$2; field_rhs=$3}
      in {region; value} }

(* Parameter of contract *)

parameter_of_type:
  "parameter_of" module_selection {
    let stop   = nsepseq_to_region (fun x -> x#region) $2 in
    let region = cover $1#region stop
    in {region; value=$2} }

(* Type qualifications

   The rule [module_path] is parameterised by what is derived after a
   series of selections of modules inside modules (nested modules),
   like [A.B.C.D]. For example, here, we want to qualify ("select") a
   type in a module, so the parameter is [type_name], because only
   types defined at top-level are in the scope (that is, any type
   declaration inside blocks is not). Then we can derive
   [A.B.C.D.t]. Notice that, in the semantic action of
   [type_in_module] we call the function [mk_mod_path] to reorganise
   the steps of the path and thus fit our CST. That complicated step
   is necessary because we need an LR(1) grammar. Indeed, rule
   [module_path] is right-recursive, yielding the reverse order of
   selection: "A.(B.(C))" instead of the expected "((A).B).C": the
   function [mk_mod_path] the semantic action of [type_in_module]
   reverses that path. We could have chosen to leave the associativity
   unspecified, like so:

     type_in_module(type_expr):
       nsepseq(module_name,".") "." type_expr { ... }

   Unfortunately, this creates a shift/reduce conflict (on "."),
   whence our more involved solution. *)

qualified_type:
  type_in_module(type_ctor { T_Var $1 }) type_ctor_args {
    let start  = type_ctor_arg_to_region $1
    and stop   = type_expr_to_region $2 in
    let region = cover start stop
    in T_App {region; value=$2,$1}
  }
| type_in_module(type_name      { T_Var $1 })
| type_in_module(par(type_expr) { T_Par $1 }) { $1 }

type_in_module(type_expr):
  module_path(type_expr) {
    T_ModPath (mk_mod_path $1 type_expr_to_region) }

(* Union or record type *)

union_or_record:
  nsep_or_pref(record(type_expr),"|") {
    match $1 with
      `Sep (record, []) -> T_Record record
    | _ -> T_Union $1 }

(* STATEMENTS *)

statements:
  nsep_or_term(statement,";") { $1 }

statement:
  base_stmt(statement)  { $1                }
| attributes if_stmt    { hook_S_Attr $1 $2 }

base_stmt(right_stmt):
  "[@attr]" base_stmt(right_stmt) { S_Attr ($1,$2) }
| no_attr_stmt(right_stmt)        { $1             }

no_attr_stmt(right_stmt):
  block_stmt               { S_Block  $1 }
| "break"                  { S_Break  $1 }
| if_else_stmt(right_stmt) { S_Cond   $1 }
| declaration              { S_Decl   $1 }
| expr                     { S_Expr   $1 }
| for_stmt(right_stmt)     { S_For    $1 }
| for_of_stmt(right_stmt)  { S_ForOf  $1 }
| return_stmt              { S_Return $1 }
| switch_stmt              { S_Switch $1 }


| while_stmt(right_stmt)   { $1 }

closed_stmt:
  base_stmt(closed_stmt) { $1 }

(* Block of statement *)

block_stmt:
  braces(statements) { $1 : statement braces }

(* Conditional statement *)

(* The reason for rules [if_cond], [while_cond] and [switch_cond],
   instead of the obvious [par(expr)], is meant to identify the
   syntactic construct for error messages. The only [par(expr)] as a
   left-hand side in an LR item corresponds to
   [member_expr: ... | par(expr)]
   so the context is clear: a general expression between parentheses. *)

if_stmt:
  "if" par(if_cond) statement {
    let region = cover $1#region (statement_to_region $3)
    and value  = {kwd_if=$1; test=$2; if_so=$3; if_not=None}
    in S_Cond {region; value} }

if_else_stmt(right_stmt):
  "if" par(if_cond) closed_stmt "else" right_stmt {
    let region = cover $1#region (statement_to_region $5)
    and value  = {kwd_if=$1; test=$2; if_so=$3; if_not = Some ($4,$5)}
    in {region; value} }

if_cond:
  expr { $1 }

(* For-loop statement *)

for_stmt(right_stmt):
  "for" par(range_for) ioption(statement) {
    let stop = match $3 with
                 None      -> $2.region
               | Some stmt -> statement_to_region stmt in
    let region = cover $1#region stop
    and value = {kwd_for=$1; test=$2; for_body=$3}
    in {region; value} }

range_for:
  ioption(initialiser) ";" ioption(condition) ";" ioption(afterthought) {
    let start = match $1 with
                  None      -> $2#region
                | Some stmt -> statement_to_region $1
    and stop = match $5 with
                 None   -> $4#region
               | Some s -> nsepseq_to_region expr_to_region s in
    let region = cover start stop
    and value = {initialiser=$1; semi1=$2; condition=$3; semi2=$4;
                 afterthought=$5}
    in {region; value} }

initialiser:
  statement { $1 }

condition:
  expr { $1 }

afterthought:
  nsepseq(expr,",") { $1 }

(* For-of loop statement *)

for_of_stmt(right_stmt):
  "for" par(range_of) right_stmt {
    let region = cover $1#region (statement_to_region $3)
    and value  = {kwd_for=$2; range=$2; for_of_body=$3}
    in {region; value} }

range_of:
  index_kind variable "of" expr {
   {index_kind=$1; index=$2; kwd_of=$3; expr=$4} }

%inline
index_kind:
  "const" { `Const $1 }
| "let"   { `Let   $1 }

(* Return statement *)

return_stmt:
  "return" expr {
    let region = cover $1#region (expr_to_region $2)
    in {region; value = ($1, Some $2)}
  }
| "return" { {region=$1#region; value = ($1, None)} }

(* Switch statement *)

switch_stmt:
  "switch" par(subject) braces(cases) {
    let region = cover $1#region $3.region
    and value  = {kwd_switch=$1; subject=$2; cases=$3}
    in {region; value} }

subject:
  expr { $1 }

cases:
  nseq(case) ioption(default_case) {
    match $2 with
      None -> $1
    | Some default ->
        Utils.(nseq_rev $1 |> nseq_cons default |> nseq_rev)
  }
| default_case { $1,[] }

case:
  "case" expr ":" ioption(case_statements) {
    Switch_case {kwd_case=$1; expr=$2; colon=$3; statements=$4} }

default_case:
  "default" ":" ioption(case_statements) {
    Switch_default_case {kwd_default=$1; colon=$2; statements=$3} }

case_statements:
  sep_or_term(case_statement,";") {
    fst $1 : (statement, semi) Utils.nsepseq }

case_statement:
  statement { $1 }
| "break"   { SBreak $1 }

(* XXX *)

(* While loop *)

while_stmt(right_stmt):
  "while" par(while_cond) right_stmt {
    let cond : expr par reg = $2 in
    let {lpar; inside=expr; rpar} : expr par = cond.value in
    let region = cover $1#region (statement_to_region $3)
    and value = {kwd_while=$1; lpar; expr; rpar; statement=$3}
    in SWhile {region; value} }

while_cond:
  expr { $1 }

for_initialiser:
  expr_stmt { $1 }

for_stmt(right_stmt):
  attributes "for" "("
    ioption(for_initialiser) ";"
    ioption(expr) ";"
    ioption(nsepseq(closed_non_decl_expr_stmt, ","))
  ")" ioption(right_stmt) {
    let initialiser = Core.Option.map $4 ~f:(fun f -> f [])
    and condition    = $6
    and afterthought = $8
    and statement    = $10 in
    let region_end   =
      match $10 with
        Some s -> statement_to_region s
      | None   -> $9#region
    in
    let region = cover $2#region region_end
    and value = {
      attributes=$1;
      kwd_for=$2;
      lpar=$3;
      initialiser;
      semi1=$5;
      condition;
      semi2=$7;
      afterthought;
      rpar=$9;
      statement;
    }
    in SFor {region;value}
  }

(* Expressions as Statements *)

expr_stmt:
  declaration                   { $1 }
| non_decl_expr_stmt(expr_stmt) { fun attrs -> SExpr (attrs, $1) }

non_decl_expr_stmt(right_stmt):
  assign_stmt                                  { EAssign $1 }
| increment_decrement_operators
| call_expr
| as_expr
| ternary_expr(non_decl_expr_stmt(right_stmt)) { $1 }

closed_non_decl_expr_stmt:
  non_decl_expr_stmt(closed_non_decl_expr_stmt) { $1 }

assign_lhs:
  projection { EProj $1 }
| variable   { EVar  $1 }

assign_stmt:
  assign_lhs "="  expr {
    $1, {value = Eq; region = $2#region}, $3 } (* TODO Wrong regions! *)
| assign_lhs "*=" expr {
    $1, {value = Assignment_operator Times_eq; region=$2#region}, $3 }
| assign_lhs "/=" expr {
    $1, {value = Assignment_operator Div_eq; region=$2#region}, $3 }
| assign_lhs "%=" expr {
    $1, {value = Assignment_operator Mod_eq; region=$2#region}, $3 }
| assign_lhs "+=" expr {
    $1, {value = Assignment_operator Plus_eq; region = $2#region}, $3 }
| assign_lhs "-=" expr {
    $1, {value = Assignment_operator Min_eq; region = $2#region}, $3 }

ternary_expr(expr):
  disj_expr_level "?" expr ":" expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $5 in
    let region = cover start stop in
    let value  = {condition=$1; qmark=$2; truthy=$3; colon=$4; falsy=$5}
    in ETernary {value; region}}

increment_decrement_operators:
  "++" variable {
    let region = cover $1#region $2#region
    and update_type = Increment $1 in
    let value = {update_type; variable=$2}
    in EPrefix {region; value}
  }
| "--" variable {
    let region = cover $1#region $2#region
    and update_type = Decrement $1 in
    let value = {update_type; variable=$2}
    in EPrefix {region; value}
  }
| variable "++" {
    let region = cover $1#region $2#region
    and update_type = Increment $2 in
    let value = {update_type; variable=$1}
    in EPostfix {region; value}
  }
| variable "--" {
    let region = cover $1#region $2#region
    and update_type = Decrement $2 in
    let value  = {update_type; variable=$1}
    in EPostfix {region; value}
  }

(* Expressions *)

expr:
  fun_expr           { EFun $1 }
| ternary_expr(expr)
| as_expr
| disj_expr_level    { $1 }

as_expr:
  call_expr_level "as" type_expr {
    let stop   = type_expr_to_region $3 in
    let region = cover (expr_to_region $1) stop
    in EAnnot {region; value = $1,$2,$3} }

disj_expr_level:
  bin_op(disj_expr_level, "||", conj_expr_level) {
    ELogic (BoolExpr (Or $1)) }
| conj_expr_level { $1 }

bin_op(arg1,op,arg2):
  arg1 op arg2 {
    let region = cover (expr_to_region $1) (expr_to_region $3)
    and value  = {arg1=$1; op=$2; arg2=$3}
    in {region; value} }

conj_expr_level:
  bin_op(conj_expr_level, "&&", comp_expr_level) {
    ELogic (BoolExpr (And $1)) }
| comp_expr_level { $1 }

comp_expr_level:
  bin_op(comp_expr_level, "<", add_expr_level)  {
    ELogic (CompExpr (Lt $1)) }
| bin_op(comp_expr_level, "<=", add_expr_level) {
    ELogic (CompExpr (Leq $1)) }
| bin_op(comp_expr_level, gt, add_expr_level)   {
    ELogic (CompExpr (Gt $1)) }
| bin_op(comp_expr_level, ge, add_expr_level) {
    ELogic (CompExpr (Geq $1)) }
| bin_op(comp_expr_level, "==", add_expr_level) {
    ELogic (CompExpr (Equal $1)) }
| bin_op(comp_expr_level, "!=", add_expr_level) {
    ELogic (CompExpr (Neq $1)) }
| add_expr_level { $1 }

add_expr_level:
  bin_op(add_expr_level, "+", mult_expr_level)   {  EArith (Add $1) }
| bin_op(add_expr_level, "-", mult_expr_level)   {  EArith (Sub $1) }
| mult_expr_level                                {               $1 }

mult_expr_level:
  bin_op(mult_expr_level, "*", unary_expr_level) { EArith (Mult $1) }
| bin_op(mult_expr_level, "/", unary_expr_level) {  EArith (Div $1) }
| bin_op(mult_expr_level, "%", unary_expr_level) {  EArith (Mod $1) }
| unary_expr_level                               {               $1 }

unary_expr_level:
  "-" call_expr_level {
    let region = cover $1#region (expr_to_region $2)
    and value  = {op=$1; arg=$2}
    in EArith (Neg {region; value})
  }
| "!" call_expr_level {
    let region = cover $1#region (expr_to_region $2)
    and value  = {op=$1; arg=$2} in
    ELogic (BoolExpr (Not ({region; value})))
  }
| increment_decrement_operators
| call_expr_level { $1 }

call_expr_level:
  call_expr | member_expr { $1 }

(* Function calls *)

call_expr:
  "contract_of" "(" module_selection ")" {
    let region = cover $1#region $4#region
    in EContract {region; value=$3 }
  }
| lambda par(ioption(nsepseq(fun_arg,","))) {
    let par    = $2.value in
    let region = cover (expr_to_region $1) $2.region in
    let args   =
      match par.inside with
        None ->
          Unit {region=$2.region; value = (par.lpar, par.rpar)}
      | Some args ->
          Multiple {$2 with value = {par with inside=args}}
    in ECall {region; value = ($1, args)} }

lambda:
  call_expr
| member_expr { $1 } (* TODO: specialise *)

fun_arg:
  expr { $1 }

(* General expressions *)

member_expr:
  "_" | variable  { E_Var      $1 }
| "<int>"         { E_Int      $1 }
| "<nat>"         { E_Nat      $1 }
| "<bytes>"       { E_Bytes    $1 }
| "<string>"      { E_String   $1 }
| "<verbatim>"    { E_Verbatim $1 }
| "<mutez>"       { E_Mutez    $1 }
| ctor_expr       { EConstr  $1            }
| projection      { EProj    $1            }
| code_inj        { ECodeInj $1            }
| par(expr)       { EPar     $1            }
| module_access_e { EModA    $1            }
| array_literal   { EArray   $1            }
| object_literal  { EObject  $1            }

(* Qualified values *)

module_access_e:
  module_name "." module_var_e {
    let region = cover $1#region (expr_to_region $3)
    and value  = {module_name=$1; selector=$2; field=$3}
    in {region; value} }

module_var_e:
  module_access_e { EModA $1 }
| field_name      { EVar  $1 }
(*| projection      { EProj $1 } TODO *)

(* Code injection *)

code_inj:
  lang_name "<verbatim>" {
    let region = cover $1#region $2#region
    and value  = {language=$1; code = EString (Verbatim $2)}
    in {region; value} }

(* Tuple projection *)

projection:
  member_expr brackets(expr) {
    let region = cover (expr_to_region $1) $2.region in
    let value  = {expr=$1; selection = Component $2 }
    in {region; value}
  }
| member_expr "." field_name {
    let selection =
      FieldName {region = cover $2#region $3#region;
                 value  = {dot=$2; value=$3}} in
    let region = cover (expr_to_region $1) $3#region
    and value  = {expr=$1; selection}
    in {region; value} }

(* Constructor applications *)

ctor_expr:
  ctor "(" ctor_args? ")" {
    let region = cover $1#region $4#region
    in {region; value = ($1,$3)} }

ctor_args:
  nsepseq(ctor_arg,",") {
    let region = nsepseq_to_region expr_to_region $1
    in ESeq {region; value=$1} }

ctor_arg:
  expr { $1 }

(* Array Patterns *)

array_pattern:
  brackets(array_item_patterns) { PArray $1 }

array_item_patterns:
  array_item_pattern {
    $1, []
  }
| array_item_patterns "," array_item_pattern {
    Utils.(nsepseq_rev $1 |> nsepseq_cons $3 ($2) |> nsepseq_rev)
  }
| array_item_patterns "," array_rest_pattern  {
    Utils.(nsepseq_rev $1 |> nsepseq_cons $3 ($2) |> nsepseq_rev) }

array_item_pattern:
  "_"           { PVar (mk_wild_pattern $1) }
| var_pattern   { PVar $1 }
| array_pattern { $1 }

array_rest_pattern:
  "..." "<ident>" {
    let region = cover $1#region $2#region
    and value  = {ellipsis=$1; rest=$2}
    in PRest {region; value} }

(* DECLARATIONS *)

declaration:
  let_decl | const_decl | type_decl { $1 }


(* PATTERNS *)

binding_list:
  nsepseq(binding_initializer,",") { $1 }

binding_initializer:
  binding_pattern ioption(binding_type) "=" expr {
    let lhs_type, type_params =
      match $2 with
        None -> None, None
      | Some (a,b) -> Some a, b in
    let start  = pattern_to_region $1
    and stop   = expr_to_region $4 in
    let region = cover start stop
    and value  = {binders=$1; type_params; lhs_type; eq=$3; expr=$4}
    in {region; value} }

binding_type:
  ":" ioption(type_parameters) type_expr { ($1,$3), $2 }

type_parameters:
  chevrons(nsepseq(type_param,",")) { $1 }

binding_pattern:
  "_"            { PVar (mk_wild_pattern $1) }
| var_pattern    { PVar $1 }
| object_pattern
| array_pattern  { $1 }

var_pattern:
  attributes variable {
    let value = {variable=$2; attributes=$1}
    in {region=$2#region; value} }

(* Record patterns (a.k.a. "object patterns" in JS) *)

object_pattern:
  braces(property_patterns) { PObject $1 }

object_sep:
  ";" | "," { $1 }

property_patterns:
  property_pattern {
    $1, []
  }
| property_patterns object_sep property_pattern {
    Utils.(nsepseq_rev $1 |> nsepseq_cons $3 ($2) |> nsepseq_rev)
  }
| property_patterns object_sep object_rest_pattern {
    Utils.(nsepseq_rev $1 |> nsepseq_cons $3 $2 |> nsepseq_rev) }

property_pattern:
  "<ident>" "=" expr {
    let region = cover $1#region (expr_to_region $3)
    and value  = {property=$1; eq=$2; value=$3}
    in PAssign {region; value}
  }
| "<ident>" ":" binding_initializer {
    let region = cover $1#region $3.region
    and value  = {property=$1; colon=$2; target=$3}
    in PDestruct {region; value}
  }
| var_pattern { PVar $1 }

object_rest_pattern:
  "..." "<ident>" {
    let region = cover $1#region $2#region
    and value  = {ellipsis=$1; rest=$2}
    in PRest {region; value} }

(* Statements *)

(* TODO: Keep terminator *)
statements:
  sep_or_term(statement,";") {
    fst $1 : (statement, semi) Utils.nsepseq }

(* Expressions *)

fun_expr:
  ioption(type_parameters) ES6FUN par(parameters)
  ioption(type_annotation) "=>" body {
    let region = cover $3.region (body_to_region $6) in
    let value  = {type_params=$1; parameters = EPar $3;
                  lhs_type=$4; arrow=$5; body=$6}
    in {region; value}
  }
| ioption(type_parameters) ES6FUN "(" ")" ioption(type_annotation) "=>" body {
    let region     = cover $3#region $4#region in
    let parameters = EUnit {region; value = ($3,$4)} in
    let region     = cover $3#region (body_to_region $7) in
    let value      = {type_params=$1; parameters; lhs_type=$5; arrow=$6; body=$7}
    in {region; value}
 }
| ES6FUN "<ident>" "=>" body
| ES6FUN "_" "=>" body {
    let region     = cover $2#region (body_to_region $4)
    and parameters = EVar $2 in
    let value = {type_params=None; parameters; lhs_type=None; arrow=$3; body=$4}
    in {region; value} }

parameters:
  nsepseq(parameter,",") {
    let region = nsepseq_to_region expr_to_region $1
    in ESeq {region; value=$1} }

(* Note: we use [expr] to avoid an LR conflict, and obtain instead
   the item
   ## par(expr) -> LPAR expr . RPAR [ ... ]
   ## parameter -> expr . type_annotation [ RPAR COMMA ]
*)

parameter:
  expr ioption(type_annotation) {
    match $2 with
      Some (colon, type_expr) ->
        let start = expr_to_region $1 in
        let stop = type_expr_to_region type_expr in
        let region = cover start stop in
        EAnnot { region; value = $1, colon, type_expr }
    | None -> $1 }

body:
  braces(statements) { FunctionBody   $1 }
| expr               { ExpressionBody $1 }

(* Tuples (a.k.a "arrays" is JS) *)

array_item:
  expr       { Expr_entry $1 }
| "..." expr {
    let region = cover $1#region (expr_to_region $2) in
    let value : array_item_rest = {ellipsis=$1; expr=$2}
    in Rest_entry {region; value} }

array_literal:
  brackets(ioption(nsepseq(array_item,","))) { $1 }

(* Records (a.k.a. "objects" in JS) *)

object_literal: (* TODO: keep the terminator *)
  braces(sep_or_term(property,object_sep) { fst $1 }) { $1 }

property:
  field_name {
    let region = $1#region in
    Punned_property {region; value = EVar $1}
  }
| attributes property_name ":" expr {
    let region = cover (expr_to_region $2) (expr_to_region $4)
    and value  = {attributes=$1; name=$2; colon=$3; value=$4}
    in Property {region; value}
  }
| "..." expr {
    let region = cover $1#region (expr_to_region $2)
    and value : property_rest = {ellipsis=$1; expr=$2}
    in Property_rest {region; value} }

property_name:
  "<int>"    { EArith  (Int $1)    }
| "<string>" { EString (String $1) }
| ctor
| field_name { EVar $1 }
