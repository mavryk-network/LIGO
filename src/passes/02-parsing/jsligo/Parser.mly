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

let (<@) = Utils.(<@)
let nseq_to_region = Nodes.nseq_to_region
let nsepseq_to_region = Nodes.nsepseq_to_region
let nsep_or_pref_to_region = Nodes.nsep_or_pref_to_region
let nseq_cons = Utils.nseq_cons

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
    let region = Nodes.nseq_to_region (fun (x,_) -> x#region) nseq in
    let region = Region.cover region (to_region field)
    and value = {module_path; selector=last_dot; field}
    in {value; region}


(* END HEADER *)
%}

(* %attribute nsepseq(case_statement,SEMI) [@recover.cost 1004] *)
(* %attribute nsepseq(statement,SEMI)      [@recover.cost 1004] *)

(* Reductions on error *)

(* %on_error_reduce gt *)
(* %on_error_reduce nseq(Attr) *)
(* %on_error_reduce bin_op(add_expr_level,PLUS,mult_expr_level) *)
(* %on_error_reduce bin_op(add_expr_level,MINUS,mult_expr_level) *)
(* %on_error_reduce app_expr_level *)
(* %on_error_reduce bin_op(disj_expr_level,BOOL_OR,conj_expr_level) *)
(* %on_error_reduce type_expr *)
(* %on_error_reduce core_type *)
(* %on_error_reduce chevrons(type_ctor_args) *)
(* %on_error_reduce disj_expr_level *)
(* %on_error_reduce core_expr *)
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
| "(" X PARAMS ")" {
    let region = cover $1#region $4#region
    and value  = {lpar=$1; inside=$2; rpar=$4}
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
  X         { $1,[] }
| X nseq(X) { let h,t = $2 in ($1, h::t) }

(* Non-empty separated sequence of items *)

nsepseq(item,sep):
  item                       { $1,[] }
| item sep nsepseq(item,sep) { let h,t = $3 in ($1, ($2,h)::t) }

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
variable        : "<ident>"  { $1 }

type_var        : "<ident>" | "<uident>" { $1 }
type_name       : "<ident>" | "<uident>" { $1 }
type_ctor       : "<ident>" | "<uident>" { $1 }
field_name      : "<ident>" | "<uident>" { $1 }
lang_name       : "<ident>" | "<uident>" { $1 }
module_name     : "<uident>" { $1 }
intf_name       : "<uident>" { $1 }
ctor            : "<uident>" { E_Ctor $1 }
file_path       : "<string>" { $1 }
record_or_tuple : "<ident>"  { E_Var $1 }

(* ENTRY POINTS *)

interactive_expr: expr EOF { $1 }

contract:
  nseq(top_decl) EOF { {decl=$1; eof=$2} }

(* TOP-LEVEL DECLARATIONS *)

top_decl:
  "<directive>"      { TL_Directive $1 }
| no_dir_top_decl    {              $1 }

no_dir_top_decl:
  declaration ";"?          { TL_Decl      ($1,$2) }
| "[@attr]" no_dir_top_decl { TL_Attr      ($1,$2) }
| "export" no_dir_top_decl  {
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
    let stop   = nsepseq_to_region (fun x -> x.region) $2 in
    let region = cover (var_kind_to_region $1) stop
    and value  = {kind=$1; bindings=$2}
    in {region; value} }

%inline
var_kind:
  "let"   { `Let   $1 }
| "const" { `Const $1 }

bindings:
  nsepseq(val_binding,",") { $1 }

val_binding:
  pattern binding_type? "=" expr {
    let region = cover (pattern_to_region $1) (expr_to_region $4) in
    let type_vars, rhs_type =
      match $2 with
        None -> None, None
      | Some (colon, (tv_opt, te)) -> tv_opt, Some (colon, te) in
    let value = {pattern=$1; type_vars; rhs_type; eq=$3; rhs_expr=$4}
    in {region; value} }

binding_type:
  type_annotation(ioption(type_vars) type_expr { $1,$2 }) { $1 }

type_annotation (right_type_expr):
  ":" right_type_expr { $1,$2 }

type_vars:
  chevrons(sep_or_term(type_var,",")) { $1 }

(* Import declaration *)

import_decl:
  "import" module_name "=" module_selection {
    let region = cover $1#region (module_selection_to_region $4)
    and value  = {kwd_import=$1; alias=$2; equal=$3; module_path=$4}
    in AliasModule {region; value}
  }
| "import" "*" "as" module_name "from" file_path {
    let region = cover $1#region $6#region
    and value  = {kwd_import=$1; times=$2; kwd_as=$3; alias=$4;
                  kwd_from=$5; file_path=$6}
    in ImportAll {region; value}
  }
| "import" braces(sep_or_term(variable, ",")) "from" file_path {
    let region = cover $1#region $4#region
    and value  = {kwd_import=$1; imported=$2; kwd_from=$3; file_path=$4}
    in ImportSome {region; value} }

module_selection:
  module_path(module_name) { M_Path (mk_mod_path $1 (fun w -> w#region)) }
| module_name              { M_Alias $1 }

module_path (selected):
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
| intf_type            { I_Type       $1 }
| intf_const           { I_Const      $1 }

intf_type:
  "type" type_name "=" type_expr {
    let value  = {kwd_type=$1; type_name=$2; type_rhs = Some ($3,$4)}
    and stop   = type_expr_to_region $4 in
    let region = cover $1#region stop
    in {region; value}
  }
| "type" type_name {
    let value  = {kwd_type=$1; type_name=$2; type_rhs=None}
    and region = cover $1#region $2#region
    in {region; value} }

intf_const:
  "const" variable type_annotation(type_expr) {
    let region = cover $1#region (type_expr_to_region (snd $3))
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
    in {region; value} }

type_binder:
  type_name | "_" { $1 }

(* TYPE EXPRESSIONS *)

type_expr:
  fun_type | variant_type | core_type { $1 }

(* Functional types *)

fun_type:
  ES6FUN fun_type_params "=>" type_expr {
    let region = cover $2.region (type_expr_to_region $4)
    in T_Fun {region; value=($2,$3,$4)} }

fun_type_params:
  par(sep_or_term(fun_type_param,",") PARAMS { $1 }) { $1 }

fun_type_param:
  variable type_annotation(type_expr) {
    let region = cover $1#region (type_expr_to_region (snd $2))
    in {region; value = ($1,$2)} }

(* Variant types *)

variant_type:
  nsepseq(variant,"|") {
    let region = nsepseq_to_region (fun x -> x.tuple.region) $1
    in T_Variant {region; value = `Sep $1}
  }
| attr_variant { $1 }

attr_variant:
  nseq("|" variant { $1,$2 }) {
    let region = nseq_to_region (fun (_,v) -> v.tuple.region) $1
    in T_Variant {region; value = `Pref $1}
  }
| "[@attr]" attr_variant { T_Attr ($1,$2) }

variant:
  "[@attr]" variant      { {$2 with attributes = $1::$2.attributes} }
| brackets(variant_comp) { {attributes=[]; tuple=$1}                }

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
  "<string>"          { T_String    $1 }
| core_type_no_string {             $1 }

core_type_no_string:
  par(type_expr)      { T_Par       $1 }
| no_par_type_expr    {             $1 }

no_par_type_expr:
  "<int>"                    { T_Int       $1 }
| "_" | type_name            { T_Var       $1 }
| type_ctor_app (type_expr)  { T_App       $1 }
| cartesian                  { T_Cart      $1 }
| parameter_of_type          { T_Parameter $1 }
| qualified_type (type_expr) {             $1 }
| attr_type
| union_or_record            {             $1 }

(* Attributed core type *)

attr_type:
  "[@attr]" core_type_no_string { T_Attr ($1,$2) }

(* Application of type arguments to type constructors *)

type_ctor_app(type_expr):
  type_ctor type_ctor_args(type_expr) {
    let region = cover $1#region $2.region
    in {region; value = (T_Var $1, $2)} }

type_ctor_args(type_expr):
  chevrons(nsep_or_term(type_ctor_arg(type_expr),",")) { $1 }

type_ctor_arg(type_expr):
  type_expr { $1 }

(* Tuples of types *)

cartesian:
  brackets(type_components) { $1 }

type_components:
  type_component_no_string {
    `Sep ($1,[])
  }
| type_component_no_string "," nsep_or_term(type_component,",") {
    Utils.nsep_or_term_cons $1 $2 $3 }

type_component_no_string:
  fun_type | variant_type | core_type_no_string { $1 }

type_component:
  type_expr { $1 }

(* Parameter of contract *)

parameter_of_type:
  "parameter_of" module_selection {
    let region = cover $1#region (module_selection_to_region $2)
    and value  = {kwd_parameter_of=$1; module_path=$2}
    in {region; value} }

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

qualified_type (type_expr):
  type_in_module(type_ctor { T_Var $1 }) type_ctor_args(type_expr) {
    let region = cover (type_expr_to_region $1) $2.region
    in T_App {region; value=$1,$2}
  }
| type_in_module (type_name { T_Var $1 }) { $1 }

type_in_module (type_expr):
  module_path (type_expr) {
    T_ModPath (mk_mod_path $1 type_expr_to_region) }

(* Union or record type *)

union_or_record:
  nsep_or_pref(record_type,"|") {
    match $1 with
     `Sep (t,[]) -> T_Record t
    | _ -> let region = nsep_or_pref_to_region (fun b -> b.region) $1
           in T_Union {region; value=$1}
  }

(* Record types (a.k.a. "object types" in JS) *)

record_type:
  braces (sep_or_term (field_type, field_sep)) { $1 }

field_type:
  field_id ioption(type_annotation (type_expr)) {
    let start = field_id_to_region $1 in
    let region =
      match $2 with
        None -> start
      | Some (_,k) -> cover start (type_expr_to_region k)
    and value = {attributes=[]; field_id=$1; field_rhs=$2}
    in {region; value}
  }
| "[@attr]" field_type {
    let attributes = ($2 : _ field reg).value.attributes in
    let value : _ field = {$2.value with attributes = $1::attributes}
    in {$2 with value} }

field_sep:
  ";" | "," { $1 }

field_id:
  field_name { F_Name $1 }
| "<int>"    { F_Int  $1 }
| "<string>" { F_Str  $1 }

(* STATEMENTS *)

statements:
  cat_stmt          after_cat_stmt
| end_with_expr     after_expr_stmt
| expr_stmt         after_expr_stmt
| empty_return_stmt (*after_expr_stmt*) after_return { nseq_cons ($1,    None) $2 }
| last_or_more (statement)          { $1 }

end_with_expr: (* and does not start with [expr] *)
  value_decl | import_decl | type_decl
| export (value_decl) | export (import_decl) | export (type_decl)
| full_return_stmt { $1 } (* [expr_stmt] is missing on purpose *)

kwd_stmt (right_stmt):
  decl_stmt | export_stmt | switch_stmt | for_of_stmt (right_stmt)
| while_stmt (right_stmt) | break_stmt | for_stmt (right_stmt)
| return_stmt | if_stmt (right_stmt) | if_else_stmt (right_stmt) { $1 }

after_cat_stmt:
  after_expr_stmt
| last_or_more (expr_stmt)    { $1 }
| expr_stmt   after_expr_stmt
| export_stmt after_expr_stmt { nseq_cons ($1, None) $2 }

after_expr_stmt:
  cat_stmt         after_cat_stmt
| decl_stmt        after_expr_stmt
| full_return_stmt after_expr_stmt { nseq_cons ($1,    None) $2 }
| last_or_more (kwd_stmt (statement)) { $1 }

after_return:
  end_with_expr     after_expr_stmt
| empty_return_stmt after_return      { nseq_cons ($1, None) $2 }
| last_or_more (kwd_stmt (statement)) { $1 }

open_cat_stmt(right_stmt):
  core_stmt (right_stmt) | if_stmt (right_stmt) | block_stmt { $1 }

cat_stmt: open_cat_stmt (cat_stmt) { $1 }

statement:
  non_if_stmt (statement) | if_stmt (statement) { $1 }

non_if_stmt (right_stmt):
  core_stmt (right_stmt) | empty_for_stmt | decl_stmt | expr_stmt
| block_stmt | export_stmt | return_stmt { $1 }

core_stmt (right_stmt):
  "[@attr]" right_stmt       { S_Attr  ($1,$2) }
| switch_stmt | for_of_stmt (right_stmt)
| while_stmt (right_stmt)  | break_stmt | full_for_stmt (right_stmt)
| if_else_stmt (right_stmt)  { $1 }

break_stmt:
  "break" { S_Break $1 }

closed_non_if_stmt:
  non_if_stmt (closed_non_if_stmt) { $1 }

last_or_more (left_stmt):
  left_stmt ";"?           { ($1,$2), [] }
| left_stmt ";" after_semi { nseq_cons ($1, Some $2) $3 }

after_semi:
  literal_expr | path_expr { (S_Expr $1, None), [] }
| statements               { $1 }

(* Export statements *)

export (right_stmt):
  "export" right_stmt {
     let region = cover $1#region (statement_to_region $2)
     in S_Export {region; value=($1,$2)} }

export_stmt:
  export (decl_stmt) { $1 }

(* Expressions as statements *)

%inline
pre_expr_stmt:
  app_expr | incr_expr | decr_expr | assign_expr
| ternary_expr (no_attr_core_expr, pre_expr_stmt) { $1       }
| par (expr)                                      { E_Par $1 }

expr_stmt: pre_expr_stmt { S_Expr $1 }

decl_stmt:
  declaration { S_Decl $1 }

(* Assignments *)

assign_expr:
  bin_op (var_path,   "=", expr) { E_Assign   $1 }
| bin_op (var_path,  "*=", expr) { E_TimesEq  $1 }
| bin_op (var_path,  "/=", expr) { E_DivEq    $1 }
| bin_op (var_path,  "%=", expr) { E_RemEq    $1 }
| bin_op (var_path,  "+=", expr) { E_AddEq    $1 }
| bin_op (var_path,  "-=", expr) { E_MinusEq  $1 }
| bin_op (var_path,  "|=", expr) { E_BitOrEq  $1 }
| bin_op (var_path,  "^=", expr) { E_BitXorEq $1 }
| bin_op (var_path,  "&=", expr) { E_BitAndEq $1 }
| bin_op (var_path, "<<=", expr) { E_BitSlEq  $1 }
| bin_op (var_path, ">>=", expr) { E_BitSrEq  $1 }

var_path:
  path (record_or_tuple) | record_or_tuple { $1 }

path (root_expr):
  root_expr nseq(selection) {
    let stop   = nseq_to_region selection_to_region $2 in
    let region = cover (expr_to_region $1) stop
    and value  = {record_or_tuple=$1; field_path=$2}
    in E_Proj {region; value} }

selection:
  "." field_name        { FieldName ($1,$2) }
| brackets ("<string>") { FieldStr       $1 }
| brackets ("<int>")    { Component      $1 }

(* Block of statements *)

block_stmt:
  braces (statements) { S_Block $1 }

(* Conditional statement *)

(* The reason for rules [if_cond], [while_cond] and [switch_cond],
   instead of the obvious [par(expr)], is meant to identify the
   syntactic construct for error messages. The only [par(expr)] as a
   left-hand side in an LR item corresponds to
   [core_expr: ... | par(expr)]
   so the context is clear: a general expression between parentheses. *)

if_stmt (right_stmt):
  "if" par(if_cond) right_stmt {
    let region = cover $1#region (statement_to_region $3)
    and value  = {kwd_if=$1; test=$2; if_so=$3; if_not=None}
    in S_Cond {region; value} }

if_else_stmt (right_stmt):
  "if" par(if_cond) closed_non_if_stmt "else" right_stmt {
    let region = cover $1#region (statement_to_region $5)
    and value  = {kwd_if=$1; test=$2; if_so=$3; if_not = Some ($4,$5)}
    in S_Cond {region; value}
  }
| "if" par(if_cond) closed_non_if_stmt "; else" right_stmt {
    let region = cover $1#region (statement_to_region $5)
    (* TODO: Append [fst $4] (";") at the end of [$3] *)
    and value  = {kwd_if=$1; test=$2; if_so=$3; if_not = Some (snd $4, $5)}
    in S_Cond {region; value} }

if_cond:
  expr { $1 }

(* For-loop statement *)

for_stmt (right_stmt):
  empty_for_stmt | full_for_stmt (right_stmt) { $1 }

empty_for_stmt:
  "for" par(range_for) {
    let region = cover $1#region $2.region in
    let value  = {kwd_for=$1; range=$2; for_body=None}
    in S_For {region; value} }

full_for_stmt (right_stmt):
  "for" par(range_for) right_stmt {
    let region = cover $1#region (statement_to_region $3) in
    let value  = {kwd_for=$1; range=$2; for_body = Some $3}
    in S_For {region; value} }

range_for:
  ioption(initialiser) ";" ioption(condition) ";" ioption(afterthought) {
    {initialiser=$1; semi1=$2; condition=$3; semi2=$4; afterthought=$5} }

initialiser:
  decl_stmt | expr_stmt { $1 }

condition:
  expr { $1 }

afterthought:
  nsepseq (expr, ",") { $1 }

(* For-of loop statement *)

for_of_stmt(right_stmt):
  "for" par(range_of) right_stmt {
    let region = cover $1#region (statement_to_region $3)
    and value  = {kwd_for=$1; range=$2; for_of_body=$3}
    in S_ForOf {region; value} }

range_of:
  index_kind variable "of" expr {
   {index_kind=$1; index=$2; kwd_of=$3; expr=$4} }

%inline
index_kind:
  "const" { `Const $1 }
| "let"   { `Let   $1 }

(* Return statement *)

return_stmt:
  full_return_stmt | empty_return_stmt { $1 }

full_return_stmt:
  "return" expr {
    let region = cover $1#region (expr_to_region $2)
    in S_Return {region; value = ($1, Some $2)} }

empty_return_stmt:
  "return" { S_Return {region=$1#region; value = ($1, None)} }

(* Switch statement *)

switch_stmt:
  "switch" par(subject) braces(cases) {
    let region = cover $1#region $3.region
    and value  = {kwd_switch=$1; subject=$2; cases=$3}
    in S_Switch {region; value} }

subject:
  expr { $1 }

cases:
  nseq(switch_case) ioption(switch_default) { AllCases ($1,$2) }
| switch_default                            { Default       $1 }

switch_case:
  "case" case_expr ":" ioption(statements) {
    let stop =
      match $4 with
        None       -> $3#region
      | Some stmts -> nseq_to_region (statement_to_region <@ fst) stmts in
    let region = cover $1#region stop
    and value  = {kwd_case=$1; expr=$2; colon=$3; case_body=$4}
    in {region; value} }

case_expr:
  ctor | literal_expr | path_expr { $1 }

switch_default:
  "default" ":" ioption(statements) {
    let stop =
      match $3 with
        None       -> $2#region
      | Some stmts -> nseq_to_region (statement_to_region <@ fst) stmts in
    let region = cover $1#region stop
    and value  = {kwd_default=$1; colon=$2; default_body=$3}
    in {region; value} }

(* While loop *)

while_stmt(right_stmt):
  "while" par(while_cond) right_stmt {
    let region = cover $1#region (statement_to_region $3)
    and value  = {kwd_while=$1; invariant=$2; while_body=$3}
    in S_While {region; value} }

while_cond:
  expr { $1 }

(* EXPRESSIONS *)

expr:
  record_level_expr | non_record_expr   { $1 }

non_record_expr:
  fun_expr | typed_expr | assign_expr | disj_expr_level
| ternary_expr (disj_expr_level, expr) { $1 }

(* Record expressions *)

record_level_expr:
  record_expr   { E_Record   $1 }
| record_update { E_Update   $1 }

(* Functional expressions *)

fun_expr:
  ioption(type_vars) ES6FUN fun_par_params ret_type? "=>" fun_body {
    let start  = match $1 with
                   None -> parameters_to_region $3
                 | Some {region; _} -> region in
    let region = cover start (fun_body_to_region $6) in
    let value  = {type_vars=$1; parameters=$3;
                  rhs_type=$4; arrow=$5; fun_body=$6}
    in E_Fun {region; value} }
| ioption(type_vars) ES6FUN fun_var_param "=>" fun_body {
    let start  = match $1 with
                   None -> $3#region
                 | Some {region; _} -> region in
    let region = cover start (fun_body_to_region $5) in
    let value  = {type_vars=$1; parameters = VarParam $3;
                  rhs_type=None; arrow=$4; fun_body=$5}
    in E_Fun {region; value} }

ret_type:
  type_annotation (ES6FUN? no_par_type_expr { $2 }) { $1 }

fun_par_params:
  par (sep_or_term(fun_param,",") PARAMS { $1 }) { ParParams $1 }

fun_var_param:
  variable | WILD { $1 }

fun_param:
  param_pattern type_annotation(type_expr) {
    let stop   = type_expr_to_region (snd $2) in
    let region = cover (pattern_to_region $1) stop
    in P_Typed {region; value = $1,$2}
  }
| param_pattern { $1 }

param_pattern:
  "_" | variable        { P_Var   $1 }
| tuple (param_pattern) { P_Tuple $1 }

fun_body:
  braces (statements) { StmtBody  $1 }
| non_record_expr     { ExprBody $1 }

(* Typed expressions *)

typed_expr:
  app_expr_level "as" type_expr {
    let stop   = type_expr_to_region $3 in
    let region = cover (expr_to_region $1) stop
    in E_Typed {region; value=($1,$2,$3)} }

(* Ternary conditional operator *)

ternary_expr (left_expr, branch):
  left_expr "?" branch ":" branch {
    let region = cover (expr_to_region $1) (expr_to_region $5)
    and value  = {condition=$1; qmark=$2; truthy=$3; colon=$4; falsy=$5}
    in E_Ternary {value; region} }

(* Logical disjunction *)

disj_expr_level:
  bin_op (disj_expr_level, "||", conj_expr_level) { E_Or     $1 }
| bin_op (disj_expr_level, "^^", conj_expr_level) { E_Xor    $1 }
| bin_op (disj_expr_level,  "|", conj_expr_level) { E_BitOr  $1 }
| bin_op (disj_expr_level,  "^", conj_expr_level) { E_BitXor $1 }
| conj_expr_level                                 {          $1 }

bin_op (arg1,op,arg2):
  arg1 op arg2 {
    let region = cover (expr_to_region $1) (expr_to_region $3)
    and value  = {arg1=$1; op=$2; arg2=$3}
    in {region; value} }

(* Logical conjunction *)

conj_expr_level:
  bin_op (conj_expr_level, "&&", bit_shift_level) { E_And    $1 }
| bin_op (conj_expr_level,  "&", bit_shift_level) { E_BitAnd $1 }
| bit_shift_level                                 {          $1 }

(* Bitwise shifts *)

bit_shift_level:
  bin_op (bit_shift_level, "<<", comp_expr_level) { E_BitSl $1 }
| bin_op (bit_shift_level, ">>", comp_expr_level) { E_BitSr $1 }
| comp_expr_level                                 {         $1 }

(* Comparisons *)

comp_expr_level:
  bin_op (comp_expr_level,  "<", add_expr_level) { E_Lt    $1 }
| bin_op (comp_expr_level, "<=", add_expr_level) { E_Leq   $1 }
| bin_op (comp_expr_level,   gt, add_expr_level) { E_Gt    $1 }
| bin_op (comp_expr_level,   ge, add_expr_level) { E_Geq   $1 }
| eq_expr_level                                  {         $1 }

eq_expr_level:
  bin_op (add_expr_level, "==", eq_expr_level) { E_Equal $1 }
| bin_op (add_expr_level, "!=", eq_expr_level) { E_Neq   $1 }
| add_expr_level                               {         $1 }

(* Addition & subtraction *)

add_expr_level:
  bin_op (add_expr_level, "+", mult_expr_level)  { E_Add $1 }
| bin_op (add_expr_level, "-", mult_expr_level)  { E_Sub $1 }
| mult_expr_level                                {       $1 }

(* Multiplications & division *)

mult_expr_level:
  bin_op (mult_expr_level, "*", unary_expr_level) { E_Mult $1 }
| bin_op (mult_expr_level, "/", unary_expr_level) { E_Div  $1 }
| bin_op (mult_expr_level, "%", unary_expr_level) { E_Rem  $1 }
| unary_expr_level                                {        $1 }

(* Logical and arithmetic negation *)

unary_expr_level:
  minus_expr | not_expr | neg_expr | incr_expr | decr_expr
| app_expr_level { $1 }

minus_expr:
  "-" app_expr_level {
    let region = cover $1#region (expr_to_region $2)
    and value  = {op=$1; arg=$2}
    in E_Neg {region; value} }

not_expr:
  "!" app_expr_level {
    let region = cover $1#region (expr_to_region $2)
    and value  = {op=$1; arg=$2}
    in E_Not {region; value} }

neg_expr:
  "~" app_expr_level {
    let region = cover $1#region (expr_to_region $2)
    and value  = {op=$1; arg=$2}
    in E_Neg {region; value} }

(* Increment & decrement operators *)

incr_expr:
  pre_incr_expr | post_incr_expr { $1 }

pre_incr_expr:
  "++" variable {
    let region = cover $1#region $2#region
    and value  = {op=$1; arg = E_Var $2}
    in E_PreIncr {region; value} }

post_incr_expr:
  variable "++" {
    let region = cover $1#region $2#region
    and value  = {op=$2; arg = E_Var $1}
    in E_PostIncr {region; value} }

decr_expr:
  pre_decr_expr | post_decr_expr { $1 }

pre_decr_expr:
  "--" variable {
    let region = cover $1#region $2#region
    and value  = {op=$1; arg = E_Var $2}
    in E_PreDecr {region; value} }

post_decr_expr:
  variable "--" {
    let region = cover $1#region $2#region
    and value  = {op=$2; arg = E_Var $1}
    in E_PostDecr {region; value} }

(* Function calls & data constructor applications *)

app_expr_level:
  contract_of_expr | app_expr | core_expr { $1 }

contract_of_expr:
  "contract_of" par(module_selection) {
    let region = cover $1#region $2.region
    and value  = {kwd_contract_of=$1; module_path=$2}
    in E_Contract {region; value} }

app_expr:
  lambda arguments {
    let region = cover (expr_to_region $1) $2.region
    in E_App {region; value=($1,$2)} }

lambda:
  app_expr | no_attr_core_expr { $1 }

arguments:
  par (ioption (nsepseq (argument, ","))) { $1 }

argument:
  expr { $1 }

(* Core expressions *)

core_expr:
  "[@attr]" core_expr { E_Attr ($1,$2) }
| no_attr_core_expr   {             $1 }

no_attr_core_expr:
  code_inj            { E_CodeInj  $1 }
| par (expr)          { E_Par      $1 }
| tuple (expr)        { E_Tuple    $1 }
| ctor
| literal_expr
| path_expr           {            $1 }

literal_expr:
  "<int>"             { E_Int      $1 }
| "<nat>"             { E_Nat      $1 }
| "<mutez>"           { E_Mutez    $1 }
| "<string>"          { E_String   $1 }
| "<verbatim>"        { E_Verbatim $1 }
| "<bytes>"           { E_Bytes    $1 }

(* Record expressions *)

record_expr:
  braces (sep_or_term (field (expr), field_sep)) { $1 }

field (right_expr):
  field_id ioption(":" right_expr { $1,$2 }) {
    let start = field_id_to_region $1 in
    let region =
      match $2 with
        None -> start
      | Some (_,k) -> cover start (expr_to_region k)
    and value : _ field = {attributes=[]; field_id=$1; field_rhs=$2}
    in {region; value}
  }
| "[@attr]" field(right_expr) {
    let attributes = ($2 : _ field reg).value.attributes in
    let value : _ field = {$2.value with attributes = $1::attributes}
    in {$2 with value} }

(* Code injection *)

code_inj:
  lang_name "<verbatim>" {
    let region = cover $1#region $2#region
    and value  = {language=$1; code = E_Verbatim $2}
    in {region; value} }

(* Functional updates of records *)

record_update:
  braces (update_expr) { $1 }

update_expr:
  "..." expr field_sep updates {
    {ellipsis=$1; record=$2; sep=$3; updates=$4} }

updates:
  sep_or_term (field (expr), field_sep) { $1 }

(* Tuples (a.k.a "arrays" is JS) *)

tuple(item):
  brackets (sep_or_term (component (item), ",")) { $1 }

component(item):
  ioption("...") item { $1,$2 }

(* Path expressions

   A path expression is a construct that qualifies unambiguously a
   value or type. When maintaining this subgrammar, be wary of not
   introducing a regression. Here are the currently possible cases:

      * a single variable: "a" or "@0" or "@type" etc.
      * a single variable in a nested module: "A.B.a"
      * nested fields and compoments from a variable: "a[0][1]b"
      * same within a nested module: "A.B.a[0][1].b"
      * nested fields and components from an expression: "(e).a[0][1]b" *)

path_expr:
  module_path (selected_expr) { E_ModPath (mk_mod_path $1 expr_to_region) }
| var_path
| path (par (expr) { E_Par $1 }) { $1 }

selected_expr:
  ctor | var_path { $1 }

(* PATTERNS *)

pattern:
  "[@attr]" pattern { P_Attr ($1,$2) }
| "<int>"           { P_Int       $1 }
| "<nat>"           { P_Nat       $1 }
| "<bytes>"         { P_Bytes     $1 }
| "<string>"        { P_String    $1 }
| "<verbatim>"      { P_Verbatim  $1 }
| "<mutez>"         { P_Mutez     $1 }
| "_" | variable    { P_Var       $1 }
| record_pattern    { P_Record    $1 }
| tuple(pattern)    { P_Tuple     $1 }

(* Record pattern *)

record_pattern:
  braces (sep_or_term (field_pattern, field_sep)) { $1 }

field_pattern:
  field_id ioption(":" pattern { $1,$2 }) {
    let start = field_id_to_region $1 in
    let region =
      match $2 with
        None -> start
      | Some (_,k) -> cover start (pattern_to_region k)
    and value : _ field = {attributes=[]; field_id=$1; field_rhs=$2}
    in {region; value}
  }
| "[@attr]" field_pattern {
    let attributes = ($2 : _ field reg).value.attributes in
    let value : _ field = {$2.value with attributes = $1::attributes}
    in {$2 with value} }
