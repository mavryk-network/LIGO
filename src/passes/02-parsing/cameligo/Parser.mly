(* Menhir specification of the parser of CameLIGO *)

%{
(* START HEADER *)

(* START HEADER *)

[@@@warning "-42"]

(* Vendors dependencies *)

open Simple_utils.Region

(* LIGO dependencies *)

module CST = Cst_pascaligo.CST
open! CST
module Wrap = Lexing_shared.Wrap

(* UTILITIES

   The following functions help build CST nodes. When they are
   complicated, like [mk_mod_path], it is because the grammar rule had
   to be written in a certain way to remain LR, and that way did not
   make it easy in the semantic action to collate the information into
   CST nodes. *)

let mk_reg region value = Region.{region; value}

let apply_type_ctor token args =
  let {region; value = {lpar; inside; rpar}} = args in
  let tuple  = mk_reg region {lpar; inside=inside,[]; rpar}
  and region = cover token#region args.region
  in mk_reg region (T_Var token, tuple)

let apply_map token args =
  let region = cover token#region args.region
  in mk_reg region (T_Var token, args)

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

(* Hooking attributes, if any *)

let rec hook mk_attr attrs node =
  match attrs with
    []            -> node
  | attr :: attrs -> mk_attr attr @@ hook mk_attr attrs node

let hook_E_Attr = hook @@ fun a e -> E_Attr (a,e)
let hook_T_Attr = hook @@ fun a t -> T_Attr (a,t)
let hook_S_Attr = hook @@ fun a s -> S_Attr (a,s)

(* Making a list of attributes from a pattern *)

let get_attributes (node : CST.pattern) =
  let rec aux attrs = function
    P_Attr (attr, pattern) -> aux (attr::attrs) pattern
  | pattern                -> List.rev attrs, pattern
  in aux [] node

(* END HEADER *)
%}

(* Reductions on error *)

%on_error_reduce seq_expr
%on_error_reduce nsepseq(selection,DOT)
%on_error_reduce call_expr_level
%on_error_reduce add_expr_level
%on_error_reduce cons_expr_level
%on_error_reduce cat_expr_level
%on_error_reduce disj_expr_level
%on_error_reduce conj_expr_level
%on_error_reduce shift_expr_level
%on_error_reduce bin_op(disj_expr_level,BOOL_OR,conj_expr_level)
%on_error_reduce bin_op(disj_expr_level,Or,conj_expr_level)
%on_error_reduce bin_op(disj_expr_level,REV_APP,conj_expr_level)
%on_error_reduce bin_op(conj_expr_level,BOOL_AND,comp_expr_level)
%on_error_reduce bin_op(comp_expr_level,ge,cat_expr_level)
%on_error_reduce bin_op(add_expr_level,PLUS,mult_expr_level)
%on_error_reduce bin_op(add_expr_level,MINUS,mult_expr_level)
%on_error_reduce base_expr(expr)
%on_error_reduce base_expr(base_cond)
%on_error_reduce base_expr(closed_expr)
%on_error_reduce module_var_e
%on_error_reduce module_var_t
%on_error_reduce nsepseq(module_name,DOT)
%on_error_reduce core_expr
%on_error_reduce match_expr(base_cond)
%on_error_reduce ctor_expr
%on_error_reduce nsepseq(disj_expr_level,COMMA)
%on_error_reduce const_ctor_expr
%on_error_reduce const_ctor_pattern
%on_error_reduce arguments
%on_error_reduce seq(Attr)
%on_error_reduce ctor_pattern
%on_error_reduce cons_pattern_level
%on_error_reduce nsepseq(cons_pattern_level,COMMA)
%on_error_reduce pattern
%on_error_reduce nsepseq(core_irrefutable,COMMA)
%on_error_reduce variant(fun_type_level)
%on_error_reduce variant(prod_type_level)
%on_error_reduce nsepseq(variant(fun_type_level),VBAR)
%on_error_reduce nsepseq(variant(prod_type_level),VBAR)
%on_error_reduce nsepseq(core_type,TIMES)
%on_error_reduce fun_type_level
%on_error_reduce prod_type_level

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <CST.t> contract
%type <CST.expr> interactive_expr

%%

(* RULES *)

(* Compound constructs *)

(* Compound constructs *)

par(X):
  "(" X ")" {
    let region = cover $1#region $3#region
    and value  = {lpar=$1; inside=$2; rpar=$3}
    in {region; value} }

brackets(X):
  "[" X "]" {
    let region = cover $1#region $3#region
    and value  = {lbracket=$1; inside=$2; rbracket=$3}
    in {region; value} }

brackes(X):
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
   corresponding to the semantic actions of those rules.
 *)

(* Non-empty sequence of items *)

nseq(X):
  X         { $1, [] }
| X nseq(X) { let hd,tl = $2 in $1, hd::tl }

(* Non-empty separated sequence of items *)

nsepseq(X,Sep):
  X                    {                 $1,        [] }
| X Sep nsepseq(X,Sep) { let h,t = $3 in $1, ($2,h)::t }

(* The rule [sep_or_term(item,sep)] ("separated or terminated list")
   parses a non-empty list of items separated by [sep], and optionally
   terminated by [sep]. The following rules were inspired by the
   following blog post by Pottier:

   http://gallium.inria.fr/blog/lr-lists/
*)

sep_or_term_list(item,sep):
  nsepseq(item,sep) {
    $1, None
  }
| nseq(item sep {$1,$2}) {
    let (first,sep), tail = $1 in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item,next_sep)::others ->
        trans ((prev_sep,item)::seq, next_sep) others in
    let list, term = trans ([],sep) tail
    in (first, List.rev list), Some term }

(* Non-empty comma-separated values (at least two values) *)

tuple(item):
  item "," nsepseq(item,",") { let h,t = $3 in $1, ($2,h)::t }

(* Lists *)

list_of(item):
  brackets(option(sep_or_term_list(item,";") { fst $1 })) { $1 }

(* Braces of *)

braces_of(item):
  braces(option(sep_or_term_list(item,";") { fst $1 })) { $1 }

(* Aliasing and inlining some tokens *)

%inline variable        : "<ident>"  { $1 }
%inline module_name     : "<uident>" { $1 }
%inline field_name      : "<ident>"  { $1 }
%inline ctor            : "<uident>" { $1 }
%inline record_or_tuple : "<ident>"  { $1 }

(* Unary operators *)

unary_op(op,arg):
  op arg {
    let region = cover $1#region (expr_to_region $2)
    and value  = {op=$1; arg=$2}
    in {region; value} }

(* Binary operators *)

bin_op(arg1,op,arg2):
  arg1 op arg2 {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in {region; value} }

(* Attributes *)

%inline
attributes:
  ioption(nseq("[@attr]") { Utils.nseq_to_list $1 }) {
    match $1 with None -> [] | Some list -> list }

(* ENTRY *)

contract:
  nseq(top_declaration) EOF { {decl=$1; eof=$2} }

top_declaration:
  declaration   { $1 }
| "<directive>" { D_Directive $1 } (* Only at top-level *)

declaration:
  type_decl   { D_Type   $1 }
| let_decl    { D_Let    $1 }
| module_decl { D_Module $1 }
| attr_decl   { D_Attr   $1 }

(* Attributed declarations *)

attr_decl:
 "[@attr]" declaration {
    let stop   = decl_to_region $2 in
    let region = cover $1.region stop
    in {region; value = ($1,$2)} }

(* Top-level type declarations *)

type_decl:
  "type" ioption(type_vars) type_name "=" type_expr {
    let region = cover $1#region (type_expr_to_region $5)
    and value  = {kwd_type=$1; params=$2; name=$3; eq=$4; type_expr=$5}
    in {region; value} }

type_vars:
  type_var             { TV_Single $1 }
| par(tuple(type_var)) { TV_Tuple  $1 }

type_var:
  "'" variable {
    let region = cover $1#region $2#region
    in {region; value = Some $1, $2}
  }
| "_" { {$1 with value = None, $1} }

(* Type expressions *)

type_expr:
  fun_type_level | variant_type(fun_type_level) { $1 }

(* The following subgrammar is _stratified_ in the usual manner to
   build in the grammar the different priorities between the syntactic
   categories. Associativity is implemented by making a rule
   left-recursive or right-recursive. This is the same technique often
   used to handle arithmetic and Boolean expressions, for instance,
   without resorting to Menhir annotations. *)

(* Functional type expressions *)

(* The recursivity to the right of the rule [fun_type_level] enforces
   the right-associativity of the arrow type constructor. So "a -> b
   -> c" is parsed as "a -> (b -> c)". *)

fun_type_level:
  cartesian_level "->" fun_type_level {
    let start  = type_expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop in
    TFun {region; value = $1,$2,$3}
  }
| cartesian_level { $1 }

(* Cartesian products *)

cartesian_level:
  core_type "*" nsepseq(core_type,"*") {
    let value  = Utils.nsepseq_cons $1 $2 $3 in
    let region = nsepseq_to_region type_expr_to_region value
    in TProd {region; value}
  }
| core_type { $1 }

(* Core types *)

core_type:
  "<string>"      { T_String $1 }
| "<int>"         { T_Int    $1 }
| "_" | type_name { T_Var    $1 }
| type_ctor_app   { T_App    $1 }
| record_type     { T_Record $1 }
| par(type_expr)  { T_Par    $1 }
| type_var        { T_Arg    $1 }
| qualified_type
| attr_type       { $1 }

(* Attributed core types *)

attr_type:
  "[@attr]" core_type { T_Attr ($1,$2) }

(* Variant types.
   We parameterise the variants by the kind of type expression that
   may occur at the rightmost side of a sentence. This enables to use
   [variant_type] in contexts that allow different types to avoid LR
   conflicts. For example, if the return type of a lambda is a
   functional type, parentheses are mandatory. *)

variant_type(right_type_expr):
  nsepseq(variant(right_type_expr),"|") {
    let region = nsepseq_to_region (fun x -> x.region) $1
    and value  = {lead_vbar=None; variants=$1}
    in T_Variant {region; value}
  }
| attributes "|" nsepseq(variant(right_type_expr),"|") {
    let region = nsepseq_to_region (fun x -> x.region) $3
    and value  = {lead_vbar = Some $2; variants=$3} in
    let t_expr = T_Variant {region; value}
    in hook_T_Attr $1 t_expr }

(* Always use [ioption] at the end of a rule *)

variant(right_type_expr):
  attributes "<uident>" ioption(of_type(right_type_expr)) {
    let stop   = match $3 with
                  None -> $2#region
                | Some (_, t) -> type_expr_to_region t in
    let region = cover $2#region stop
    and value  = {ctor; ctor_args=$3; attributes=$1}
    in {region; value} }

of_type(right_type_expr):
  "of" right_type_expr { $1,$2 }

(* Type constructor applications *)

type_ctor_app:
  core_type type_name {
    let region = cover (type_expr_to_region $1) $2#region
    in mk_reg region ($2, TC_Single $1)
  }
| par(tuple(type_expr)) type_name {
    let region = cover $1.region $2#region
    in mk_reg region ($2, TC_Tuple $1) }

(* Record types *)

record_type:
  braces_of(field_decl) { $1 }

(* When the type annotation is missing in a field declaration, the
   type of the field is the name of the field. *)

field_decl:
  attributes field_name ioption(type_annotation) {
    let stop = match $3 with
                        None -> $2#region
               | Some (_, t) -> type_expr_to_region t in
    let region = match $1 with
                         [] -> cover $2#region stop
                 | start::_ -> cover start.region stop
    and value = {attributes=$1; field_name=$2; field_type=$3}
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

qualified_type:
  type_in_module(type_ctor) type_tuple {
    let region = cover (type_expr_to_region $1) $2.region
    in T_App {region; value=$1,$2}
  }
| type_in_module(type_name      { T_Var $1 })
| type_in_module(par(type_expr) { T_Par $1 }) { $1 }

type_in_module(type_expr):
  module_path(type_expr) {
    T_ModPath (mk_mod_path $1 type_expr_to_region) }

module_path(selected):
  module_name "." module_path(selected) {
    let (head, tail), selected = $3 in
    (($1,$2), head::tail), selected
  }
| module_name "." selected { (($1,$2), []), $3 }

(* Top-level value declarations *)

let_decl:
  "let" ioption("rec") let_binding {
    let stop   = expr_to_region $3.let_rhs in
    let region = cover $1#region stop
    in mk_reg region ($1,$2,$3) }

let_binding:
  var_pattern type_params parameters rhs_type "=" expr {
    let binders = Utils.nseq_cons (PVar $1) $3 in
    {binders; type_params=$2; rhs_type=$4; eq=$5; let_rhs=$6}
  }
| irrefutable type_params rhs_type "=" expr {
    {binders=($1,[]); type_params=$2; rhs_type=$3; eq=$4; let_rhs=$5} }

%inline
rhs_type:
  ioption(type_annotation(type_expr)) { $1 }

type_annotation(right_type_expr):
  ":" right_type_expr { $1,$2 }

%inline (* This %inline solves a shift/reduce conflict. *)
type_params:
  ioption(par("type" nseq(variable) { $1,$2 })) { $1 }

parameters:
  nseq(core_irrefutable) { $1 }

(* Top-level module declaration *)

module_decl:
  "module" module_name "=" module_expr {
    let region = cover $1#region $4#region
    and value  = {kwd_module=$1; name=$2; eq=$3; module_expr=$4}
    in {region; value} }

module_expr:
  structure                { M_Body $1 }
| module_name              { M_Var  $1 }
| module_path(module_name) {
    M_Path (mk_mod_path $1 (fun x -> x#region)) }

structure:
  "struct" nseq(declaration)? "end" {
    {kwd_struct=$1; declarations=$2; kwd_end=$3} }

module_path(selected):
  module_name "." module_path(selected) {
    let (head, tail), selected = $3 in
    (($1,$2), head::tail), selected
  }
| module_name "." selected { (($1,$2), []), $3 }





(* PATTERNS *)

(* Irrefutable Patterns *)

%inline
irrefutable:
  tuple(core_irrefutable) {
    let region = nsepseq_to_region pattern_to_region $1
    in PTuple {region; value=$1}
  }
| core_irrefutable { $1 }

%inline
core_irrefutable:
  "_" | var_pattern           { PVar    $1 }
| unit                        { PUnit   $1 }
| record_pattern(irrefutable) { PRecord $1 }
| par(typed_irrefutable)
| par(irrefutable)            { PPar    $1 }
| ctor_irrefutable            { $1 }

var_pattern:
  attributes "<ident>" {
    let variable = unwrap $2 in
    let value = {variable; attributes=$1}
    in {variable with value} }

typed_irrefutable:
  irrefutable type_annotation(type_expr) {
    let colon, type_expr = $2 in
    let start  = pattern_to_region $1 in
    let stop   = type_expr_to_region type_expr in
    let region = cover start stop in
    let value  = {pattern=$1; colon; type_expr}
    in PTyped {region; value} }

ctor_irrefutable:
  par(non_const_ctor_irrefutable) {    PPar $1 }
| const_ctor_pattern              { PConstr $1 }

const_ctor_pattern:
  "<uident>" {
    let ctor = unwrap $1 in
    {ctor with value = ctor,None} }

non_const_ctor_irrefutable:
  "<uident>" core_irrefutable {
    let ctor = unwrap $1 in
    let stop   = pattern_to_region $2 in
    let region = cover ctor.region stop
    and value  = ctor, Some $2 in
    PConstr {region; value} }

(* General Patterns *)

pattern:
  tuple(cons_pattern_level) {
    let region = nsepseq_to_region pattern_to_region $1
    in PTuple {region; value=$1} }
| cons_pattern_level { $1 }

cons_pattern_level:
  core_pattern "::" cons_pattern_level {
    let start  = pattern_to_region $1 in
    let colons = $2 in
    let stop   = pattern_to_region $3 in
    let region = cover start stop in
    PList (PCons {region; value=$1,colons,$3}) }
| core_pattern { $1 }

core_pattern:
  var_pattern                     { PVar $1 }
| "_"                             { PVar (mk_wild $1#region) }
| "<int>"                         {                           PInt (unwrap $1) }
| "<nat>"                         {                           PNat (unwrap $1) }
| "<bytes>"                       {                         PBytes (unwrap $1) }
| "<string>"                      {                        PString (unwrap $1) }
| "<verbatim>"                    {                      PVerbatim (unwrap $1) }
| unit                            {                          PUnit $1 }
| list_of(cons_pattern_level)     {              PList (PListComp $1) }
| ctor_pattern                  {                        PConstr $1 }
| record_pattern(core_pattern)    {                        PRecord $1 }
| par(pattern)
| par(typed_pattern)              { PPar $1}

typed_pattern:
  pattern type_annotation(type_expr) {
    let colon, type_expr = $2 in
    let start  = pattern_to_region $1 in
    let stop   = type_expr_to_region type_expr in
    let region = cover start stop in
    let value  = {pattern=$1; colon; type_expr}
    in PTyped {region; value} }

record_pattern(rhs_pattern):
  "{" sep_or_term_list(field_pattern(rhs_pattern),";") "}" {
    let lbrace = $1 in
    let rbrace = $3 in
    let ne_elements, terminator = $2 in
    let region = cover lbrace#region rbrace#region in
    let value  = {
      compound = Some (Braces (lbrace,rbrace));
      ne_elements;
      terminator;
      attributes=[]}
    in {region; value} }

field_pattern(rhs_pattern):
  field_name {
    let region  = $1.region in
    let pattern = PVar {region; value = {variable=$1; attributes=[]}} in
    let value   = {field_name=$1; eq=wrap "" Region.ghost; pattern}
    in {region; value}
  }
| field_name "=" rhs_pattern {
    let eq = $2 in
    let start  = $1.region
    and stop   = pattern_to_region $3 in
    let region = cover start stop
    and value  = {field_name=$1; eq; pattern=$3}
    in {region; value} }

ctor_pattern:
  "<uident>" ioption(core_pattern) {
    let ctor = unwrap $1 in
    let region =
      match $2 with
        None -> ctor.region
      | Some stop -> cover ctor.region (pattern_to_region stop)
    in {region; value = (ctor,$2)} }

unit:
  "(" ")" { {region = cover $1#region $2#region; value = $1,$2} }

(* Expressions *)

interactive_expr:
  expr EOF { $1 }

expr:
  base_cond__open(expr) | match_expr(base_cond) { $1 }

base_cond__open(x):
  base_expr(x) | conditional(x) { $1 }

base_cond:
  base_cond__open(base_cond) { $1 }

base_expr(right_expr):
  tuple_expr
| let_in_expr(right_expr)
| local_type_decl(right_expr)
| local_module_decl(right_expr)
| local_module_alias(right_expr)
| fun_expr(right_expr)
| disj_expr_level { $1 }

tuple_expr:
  tuple(disj_expr_level) {
    let region = nsepseq_to_region expr_to_region $1
    in ETuple {region; value=$1} }

conditional(right_expr):
  if_then_else(right_expr) | if_then(right_expr) { $1 }

if_then_else(right_expr):
  "if" expr "then" closed_expr "else" right_expr {
    let kwd_if = $1 in
    let kwd_then = $3 in
    let kwd_else = $5 in
    let region = cover kwd_if#region (expr_to_region $6)
    and value  = {kwd_if;
                  test     = $2;
                  kwd_then;
                  ifso     = $4;
                  ifnot    = Some(kwd_else,$6)}
    in ECond {region; value} }

if_then(right_expr):
  "if" expr "then" right_expr {
    let kwd_if = $1 in
    let kwd_then = $3 in
    let stop     = expr_to_region $4 in
    let region   = cover kwd_if#region stop in
    let value    = {kwd_if;
                    test     = $2;
                    kwd_then;
                    ifso     = $4;
                    ifnot    = None}
    in ECond {region; value} }

base_if_then_else__open(x):
  base_expr(x) | if_then_else(x) { $1 }

base_if_then_else:
  base_if_then_else__open(base_if_then_else) { $1 }

closed_expr:
  base_if_then_else__open(closed_expr)
| match_expr(base_if_then_else) { $1 }

match_expr(right_expr):
  "match" expr "with" "|"? cases(right_expr) {
    let kwd_match = $1 in
    let kwd_with = $3 in
    let lead_vbar = $4 in
    let cases: ('a case_clause reg, vbar) Utils.nsepseq reg = {
      value  = Utils.nsepseq_rev $5;
      region = nsepseq_to_region (fun x -> x.region) $5}
    and stop =
      match $5 with
        {region; _}, [] -> region
      |           _, tl -> last (fun f -> (fst f)#region) tl in
    let region = cover kwd_match#region stop
    and value  = {kwd_match;
                  expr      = $2;
                  kwd_with;
                  lead_vbar;
                  cases}
    in ECase {region; value} }

cases(right_expr):
  case_clause(right_expr) {
    let start  = pattern_to_region $1.pattern
    and stop   = expr_to_region $1.rhs in
    let region = cover start stop
    in {region; value=$1}, []
  }
| cases(base_cond) "|" case_clause(right_expr) {
    let start =
      match $1 with
         only_case, [] -> only_case.region
      | _, other_cases -> last (fun f -> (fst f)#region) other_cases
    and stop             = expr_to_region $3.rhs in
    let region           = cover start stop in
    let fst_case         = {region; value=$3}
    and snd_case, others = $1
    in fst_case, ($2,snd_case)::others }

case_clause(right_expr):
  pattern "->" right_expr {
    {pattern=$1; arrow=$2; rhs=$3} }

let_in_expr(right_expr):
   attributes "let" ioption("rec") let_binding "in" right_expr  {
    let kwd_let = $2 in
    let kwd_rec = $3 in
    let kwd_in  = $5 in
    let stop   = expr_to_region $6 in
    let region = cover kwd_let#region stop
    and value  = {attributes = $1;
                  kwd_let;
                  kwd_rec;
                  binding    = $4;
                  kwd_in;
                  body       = $6}
    in ELetIn {region; value} }

local_type_decl(right_expr):
  type_decl "in" right_expr  {
    let stop   = expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {type_decl  = $1.value;
                  kwd_in     = $2;
                  body       = $3}
    in ETypeIn {region; value} }

local_module_decl(right_expr):
  module_decl "in" right_expr  {
    let stop   = expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {mod_decl  = $1.value;
                  kwd_in    = $2;
                  body      = $3}
    in EModIn {region; value} }

local_module_alias(right_expr):
  module_alias "in" right_expr  {
    let stop   = expr_to_region $3 in
    let region = cover $1.region stop
    and value  = {mod_alias = $1.value;
                  kwd_in    = $2;
                  body      = $3}
    in EModAlias {region; value} }

fun_expr(right_expr):
  attributes "fun" type_parameters nseq(core_irrefutable) ret_type "->" right_expr {
    let kwd_fun = $2 in
    let stop   = expr_to_region $7 in
    let region = cover kwd_fun#region stop in
    let value  = {kwd_fun; type_params=$3; binders=$4;
                  rhs_type=$5; arrow=$6; body=$7; attributes=$1}
    in EFun {region; value} }

%inline ret_type:
  ioption(type_annotation(lambda_app_type)) { $1 }

lambda_app_type:
  cartesian_level | variant_type(cartesian_level) { $1 }

disj_expr_level:
  bin_op(disj_expr_level, "||", conj_expr_level)
| bin_op(disj_expr_level, "or", conj_expr_level) {
    ELogic (BoolExpr (Or $1))
  }
| bin_op(disj_expr_level, "|>", conj_expr_level) {
    ERevApp $1
  }
| conj_expr_level { $1 }

bin_op(arg1,op,arg2):
  arg1 op arg2 {
    let start  = expr_to_region $1 in
    let stop   = expr_to_region $3 in
    let op     = $2 in
    let region = cover start stop
    and value  = {arg1=$1; op; arg2=$3}
    in {region; value} }

conj_expr_level:
  bin_op(conj_expr_level, "&&", comp_expr_level) {
    ELogic (BoolExpr (And $1)) }
| comp_expr_level { $1 }

comp_expr_level:
  bin_op(comp_expr_level, "<", cat_expr_level) {
    ELogic (CompExpr (Lt $1)) }
| bin_op(comp_expr_level, "<=", cat_expr_level) {
    ELogic (CompExpr (Leq $1)) }
| bin_op(comp_expr_level, ">", cat_expr_level) {
    ELogic (CompExpr (Gt $1)) }
| bin_op(comp_expr_level, ge, cat_expr_level) {
    ELogic (CompExpr (Geq $1)) }
| bin_op(comp_expr_level, "=", cat_expr_level) {
    ELogic (CompExpr (Equal $1)) }
| bin_op(comp_expr_level, "<>", cat_expr_level) {
    ELogic (CompExpr (Neq $1)) }
| cat_expr_level { $1 }

ge:
  ">" ZWSP "=" { Wrap.wrap ">=" (cover $1#region $3#region) }

cat_expr_level:
  bin_op(cons_expr_level, "^", cat_expr_level)     { EString (Cat $1) }
| cons_expr_level                                  {               $1 }

cons_expr_level:
  bin_op(add_expr_level, "::", cons_expr_level)    { EList (ECons $1) }
| add_expr_level                                   {               $1 }

add_expr_level:
  bin_op(add_expr_level, "+", mult_expr_level)     {  EArith (Add $1) }
| bin_op(add_expr_level, "-", mult_expr_level)     {  EArith (Sub $1) }
| mult_expr_level                                  {               $1 }

mult_expr_level:
  bin_op(mult_expr_level, "*", shift_expr_level)    {  EArith (Mult $1) }
| bin_op(mult_expr_level, "/", shift_expr_level)    {   EArith (Div $1) }
| bin_op(mult_expr_level, "mod", shift_expr_level)  {   EArith (Mod $1) }
| bin_op(mult_expr_level, "land", shift_expr_level) {  EArith (Land $1) }
| bin_op(mult_expr_level, "lor", shift_expr_level)  {   EArith (Lor $1) }
| bin_op(mult_expr_level, "lxor", shift_expr_level) {  EArith (Lxor $1) }
| shift_expr_level                                  {                $1 }

shift_expr_level:
  bin_op(unary_expr_level, "lsl", shift_expr_level) { EArith (Lsl $1) }
| bin_op(unary_expr_level, "lsr", shift_expr_level) { EArith (Lsr $1) }
| unary_expr_level                                  { $1 }

unary_expr_level:
  "-" call_expr_level {
    let op = $1 in
    let start = op#region in
    let stop = expr_to_region $2 in
    let region = cover start stop
    and value  = {op; arg=$2}
    in EArith (Neg {region; value})
  }
| "not" call_expr_level {
    let op = $1 in
    let start = op#region in
    let stop = expr_to_region $2 in
    let region = cover start stop
    and value  = {op; arg=$2} in
    ELogic (BoolExpr (Not {region; value}))
  }
| call_expr_level { $1 }

call_expr_level:
  call_expr | core_expr | ctor_expr { $1 }

ctor_expr:
  "<uident>" argument {
    let ctor = unwrap $1 in
    let region = cover ctor.region (expr_to_region $2)
    in EConstr {region; value = (ctor, Some $2)}
  }
| const_ctor_expr { $1 }

const_ctor_expr:
  "<uident>" {
    let ctor = unwrap $1 in
    EConstr {ctor with value=(ctor,None)} }

arguments:
  argument           { $1,[]                      }
| argument arguments { let h,t = $2 in ($1, h::t) }

argument:
  core_expr
| const_ctor_expr { $1 }

call_expr:
  core_expr arguments {
    let start  = expr_to_region $1 in
    let stop   = match $2 with
                   e, [] -> expr_to_region e
                 | _,  l -> last expr_to_region l in
    let region = cover start stop in
    ECall {region; value=$1,$2} }

core_expr:
  "<int>"                             {               EArith (Int (unwrap $1)) }
| "<mutez>"                           {             EArith (Mutez  (unwrap $1)) }
| "<nat>"                             {               EArith (Nat  (unwrap $1)) }
| "<bytes>"                           {                     EBytes (unwrap $1) }
| "<ident>"                           {                       EVar (unwrap $1) }
| projection                          {                      EProj $1 }
| module_access_e                     {                      EModA $1 }
| "<string>"                          {           EString (String (unwrap $1))}
| "<verbatim>"                        {         EString (Verbatim (unwrap $1)) }
| unit                                {                      EUnit $1 }
| list_of(expr)                       {          EList (EListComp $1) }
| sequence                            {                       ESeq $1 }
| record_expr                         {                    ERecord $1 }
| update_record                       {                    EUpdate $1 }
| code_inj                            {                   ECodeInj $1 }
| par(expr)                           {                       EPar $1 }
| par(typed_expr)                     {                     EAnnot $1 }

code_inj:
  "[%lang" expr "]" {
    let region = cover $1.region $3#region
    and value  = {language=$1; code=$2; rbracket=$3}
    in {region; value} }

typed_expr:
  expr ":" type_expr { $1,$2,$3 }

projection:
  struct_name "." nsepseq(selection,".") {
    let start  = $1.region in
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover start stop in
    let value  = {struct_name=$1; selector=$2; field_path=$3}
    in {region; value} }

module_access_e:
  module_name "." module_var_e {
    let start       = $1.region in
    let stop        = expr_to_region $3 in
    let region      = cover start stop in
    let value       = {module_name=$1; selector=$2; field=$3}
    in {region; value} }

module_var_e:
  module_access_e   { EModA $1 }
| "or"              { EVar {value="or"; region=$1#region} }
| field_name        { EVar  $1 }
| projection        { EProj $1 }

selection:
  field_name { FieldName $1 }
| "<int>"    { Component (unwrap $1) }

record_expr:
  "{" sep_or_term_list(field_assignment,";") "}" {
    let lbrace = $1 in
    let rbrace = $3 in
    let ne_elements, terminator = $2 in
    let region = cover lbrace#region rbrace#region in
    let value  = {compound = Some (Braces (lbrace,rbrace));
                  ne_elements;
                  terminator;
                  attributes=[]}
    in {region; value} }

update_record:
  "{" path "with" sep_or_term_list(field_path_assignment,";") "}" {
    let lbrace = $1 in
    let kwd_with = $3 in
    let rbrace = $5 in
    let region = cover lbrace#region rbrace#region in
    let ne_elements, terminator = $4 in
    let value = {
      lbrace;
      record   = $2;
      kwd_with;
      updates  = {value = {compound = None;
                           ne_elements;
                           terminator;
                           attributes=[]};
                  region = cover kwd_with#region rbrace#region};
      rbrace}
    in {region; value} }

field_path_assignment:
  field_name {
     let region = $1.region
      and value  = Path_punned_property $1
        in {region; value}
  }
| path "=" expr {
    let region = cover (path_to_region $1) (expr_to_region $3)
    and value  = Path_property {field_path=$1; assignment=$2; field_expr=$3}
    in {region; value} }

field_assignment:
  field_name {
    let region = $1.region
    and value  = Punned_property $1
      in {region; value}
  }
| field_name "=" expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = Property {field_name=$1; assignment=$2; field_expr=$3}
    in {region; value} }

path:
 "<ident>"   { Name (unwrap $1) }
| projection { Path $1 }

(* Sequences *)

sequence:
  "begin" ioption(series) "end" {
    let begin_ = $1 in
    let end_ = $3 in
    let region   = cover begin_#region end_#region
    and compound = Some (BeginEnd (begin_,end_)) in
    let elements = $2 in
    let value    = {compound; elements; terminator=None}
    in {region; value} }

series:
  seq_expr ";" series { Utils.nsepseq_cons $1 $2 $3 }
| last_expr           { $1,[] }

last_expr:
  seq_expr
| fun_expr(last_expr)
| match_expr(last_expr)
| let_in_sequence
| tuple_expr { $1 }

let_in_sequence:
  attributes "let" ioption("rec") let_binding "in" series  {
    let seq      = $6 in
    let stop     = nsepseq_to_region expr_to_region seq in
    let kwd_let  = $2 in
    let kwd_rec  = $3 in
    let kwd_in   = $5 in
    let region   = cover kwd_let#region stop in
    let compound = None in
    let elements = Some seq in
    let value    = {compound; elements; terminator=None} in
    let body     = ESeq {region; value} in
    let value    = {attributes = $1;
                    kwd_let;
                    kwd_rec;
                    binding    = $4;
                    kwd_in;
                    body}
    in ELetIn {region; value} }

seq_expr:
  disj_expr_level | if_then_else (seq_expr) { $1 }
