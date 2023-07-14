(* PRINTING THE CST *)

[@@@coverage exclude_file]

(* Vendor dependencies *)

module Directive = Preprocessor.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

(* Internal dependencies *)

module Attr = Lexing_shared.Attr
module Tree = Cst_shared.Tree

open CST (* THE ONLY GLOBAL OPENING *)

(* UTILITIES *)

let (<@) = Utils.(<@)

let print_attribute state (node : Attr.t wrap) =
  let key, val_opt = node#payload in
  match val_opt with
    None ->
      Tree.(make_unary state "<attribute>" make_node key)
  | Some String value ->
      let children = Tree.[
        mk_child make_node key;
        mk_child make_node (Printf.sprintf "%S" value)]
      in Tree.make state "<attribute>" children
  | Some Ident value ->
      let children = Tree.[
        mk_child make_node key;
        mk_child make_node value]
      in Tree.make state "<attribute>" children

let mk_children_attr (node : Attr.t wrap list) =
  Tree.mk_children_list print_attribute ~root:"<attributes>" node

(* Preprocessing directives *)

let print_TL_Directive state (node : Directive.t) =
  let region, string = Directive.project node in
  Tree.(make_unary state "Directive" make_node ~region string)

(* PRINTING THE CST *)

let rec print_cst state (node : cst) =
  Tree.of_nseq state "<cst>" print_top_decl node.decl

(* TOP-LEVEL *)

and print_top_decl state = function
  TL_Decl      d -> print_TL_Decl      state d
| TL_Attr      d -> print_TL_Attr      state d
| TL_Export    d -> print_TL_Export    state d
| TL_Directive d -> print_TL_Directive state d

(* Normal declaration *)

and print_TL_Decl state (d, _) = print_declaration state d

(* Attributed declaration *)

and print_TL_Attr state (node: attribute * top_decl) =
  let attribute, top_decl = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_top_decl  top_decl]
  in Tree.make state "TL_Attr" children

(* Export declaration *)

and print_TL_Export state (node : (kwd_export * top_decl) reg) =
  let Region.{region; value} = node in
  Tree.make_unary ~region state "TL_Export" print_top_decl (snd value)

(* INNER DECLARATIONS (AS STATEMENTS) *)

and print_declaration state = function
  D_Value     d -> print_D_Value     state d
| D_Import    d -> print_D_Import    state d
| D_Interface d -> print_D_Interface state d
| D_Module    d -> print_D_Module    state d
| D_Type      d -> print_D_Type      state d

(* Value declaration *)

and print_D_Value state (node: value_decl reg) =
  let Region.{region; value} = node in
  let {kind; bindings} = value in
  let children = Tree.mk_child print_var_kind kind
                 :: mk_children_bindings bindings
  in Tree.make state ~region "D_Value" children

and print_var_kind state = function
  `Let   kwd_let   -> Tree.make_literal state kwd_let
| `Const kwd_const -> Tree.make_literal state kwd_const

and mk_children_bindings (node: (val_binding reg, comma) sep_or_term) =
  Tree.mk_children_sep_or_term print_val_binding node

and print_val_binding state (node: val_binding reg) =
  let Region.{region; value} = node in
  let {pattern; type_vars; rhs_type; eq=_; rhs_expr} = value in

  let children = Tree.[
    mk_child     print_binders         pattern;
    mk_child_opt print_type_vars       type_vars;
    mk_child_opt print_type_annotation rhs_type;
    mk_child     print_rhs             rhs_expr]
  in Tree.make state ~region "<binding>" children

and print_rhs state (node: expr) =
  Tree.make_unary state "<rhs>" print_expr node

and print_binders state (node: pattern) =
  Tree.make_unary state "<binders>" print_pattern node

and print_type_vars state (node: type_vars) =
  let Region.{region; value} = node in
  let seq = value.inside in
  Tree.(of_sep_or_term state ~region "<type vars>" make_literal seq)

and print_type_annotation state (node : type_annotation) =
  Tree.make_unary state "<type>" print_type_expr (snd node)

(* Import declaration *)

and print_D_Import state (node: import_decl) =
  Tree.make_unary state "D_Import" print_import_decl node

and print_import_decl state = function
  AliasModule d -> print_AliasModule state d
| ImportAll   d -> print_ImportAll   state d
| ImportSome  d -> print_ImportSome  state d

and print_AliasModule state (node: import_alias reg) =
  let Region.{value; region} = node in
  let {kwd_import=_; alias; equal=_; module_path} = value in
  let print_module_path =
    print_module_path Tree.make_literal "<module_path>" in
  let children = Tree.[
    mk_child print_alias       alias;
    mk_child print_module_path module_path]
  in Tree.make ~region state "AliasModule" children

and print_alias state (node: module_name) =
  Tree.(make_unary state "<alias>" make_literal node)

and print_module_path :
  'a.'a Tree.printer -> Tree.root -> Tree.state -> 'a module_path reg -> unit =
  fun print root state {value; region} ->
    let children = Tree.(
        mk_children_nsepseq make_literal value.module_path
      @ [Tree.mk_child print value.field])
    in Tree.make state root ~region children

and print_ImportAll state (node: import_all_as reg) =
  let Region.{value; region} = node in
  let {kwd_import=_; times=_; kwd_as=_; alias;
       kwd_from=_; file_path} = value in
  let children = Tree.[
    mk_child print_alias     alias;
    mk_child print_file_path file_path]
  in Tree.make ~region state "ImportAll" children

and print_file_path state (node: string wrap) =
  Tree.(make_unary state "<file path>" make_literal node)

and print_ImportSome state (node: import_from reg) =
  let Region.{value; region} = node in
  let {kwd_import=_; imported; kwd_from=_; file_path} = value in
  let children = Tree.[
    mk_child print_imported  imported;
    mk_child print_file_path file_path]
  in Tree.make ~region state "ImportSome" children

and print_imported state (node: (field_name, comma) sep_or_term braces) =
  let Region.{region; value} = node in
  Tree.(of_sep_or_term state ~region "<module path>"
                       make_literal value.inside)

(* Interface declaration *)

and print_D_Interface state (node : interface_decl reg) =
  let Region.{region; value} = node in
  let {kwd_interface=_; intf_name; intf_body} = value in
  let children = Tree.(mk_child make_literal intf_name)
                 :: mk_children_intf_body intf_body
  in Tree.make ~region state "D_Interface" children

and mk_children_intf_body (node: intf_body) =
  Tree.mk_children_sep_or_term print_intf_entry node.value.inside

and print_intf_entry state = function
  I_Attr  e -> print_I_Attr  state e
| I_Type  e -> print_I_Type  state e
| I_Const e -> print_I_Const state e

and print_I_Attr state (node : attribute * intf_entry) =
  let attribute, entry = node in
  let children = Tree.[
    mk_child print_attribute  attribute;
    mk_child print_intf_entry entry]
  in Tree.make state "I_Attr" children

and print_I_Type state (node : intf_type reg) =
  let Region.{region; value} = node in
  let {kwd_type=_; type_name; type_rhs} = value in
  let children = Tree.[
    mk_child     make_literal   type_name;
    mk_child_opt print_type_rhs type_rhs]
  in Tree.make ~region state "I_Type" children

and print_type_rhs state (node : equal * type_expr) =
  print_type_expr state (snd node)

and print_I_Const state (node : intf_const reg) =
  let Region.{value; region} = node in
  let {kwd_const=_; const_name; const_type} = value in
  let children = Tree.[
    mk_child make_literal          const_name;
    mk_child print_type_annotation const_type]
  in Tree.make ~region state "I_Const" children

(* Modules *)

and print_D_Module state (node: module_decl reg) =
  let Region.{value; region} = node in
  let {kwd_namespace=_; module_name; module_type; module_body} = value in
  let children = Tree.[
    mk_child     print_module_name module_name;
    mk_child_opt print_interface   module_type;
    mk_child     print_statements  module_body.value.inside]
  in Tree.make ~region state "D_Module" children

and print_module_name state (node: module_name) =
  Tree.(make_unary state "<module>" make_literal node)

and print_interface state (node: interface) =
  let Region.{value; region} = node in
  let _kwd_implements, intf_expr = value in
  Tree.make_unary ~region state "<interface>" print_intf_expr intf_expr

and print_intf_expr state = function
  I_Body i -> print_I_Body state i
| I_Path i -> print_I_Path state i

and print_I_Body state (node: intf_body) =
  Tree.make state "I_Body" (mk_children_intf_body node)
and print_I_Path state (node: module_selection) =
  print_module_path Tree.make_literal "I_Path" state node

(* Type declaration *)

and print_D_Type state (node: type_decl reg) =
  let Region.{value; region} = node in
  let {kwd_type=_; type_vars; name; eq=_; type_expr} = value in
  let children = Tree.[
    mk_child     make_literal    name;
    mk_child_opt print_type_vars type_vars;
    mk_child     print_type_expr type_expr]
  in Tree.make state ~region "D_Type" children

(* TYPE EXPRESSIONS *)

and print_type_expr state = function
  T_App       t -> print_T_App       state t
| T_Attr      t -> print_T_Attr      state t
| T_Cart      t -> print_T_Cart      state t
| T_Fun       t -> print_T_Fun       state t
| T_Int       t -> print_T_Int       state t
| T_ModPath   t -> print_T_ModPath   state t
| T_Par       t -> print_T_Par       state t
| T_Parameter t -> print_T_Parameter state t
| T_Record    t -> print_T_Record    state t
| T_String    t -> print_T_String    state t
| T_Union     t -> print_T_Union     state t
| T_Var       t -> print_T_Var       state t
| T_Variant   t -> print_T_Variant   state t

(* Application of type constructors *)

and print_T_App state (node: (type_ctor * type_ctor_args) reg) =
  let Region.{region; value} = node in
  let type_ctor, args = value in
  let args = args.value.inside in
  let children = Tree.(
    mk_child make_literal type_ctor
    :: mk_children_nsep_or_term ~root:"<arguments>" print_type_expr args)
  in Tree.make state ~region "T_App" children

(* Attributed type expression *)

and print_T_Attr state (node : attribute * type_expr) =
  let attribute, type_expr = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_type_expr type_expr]
  in Tree.make state "T_Attr"children

(* Cartesian products *)

and print_T_Cart state (node : cartesian) =
  let Region.{value; region} = node in
  Tree.of_nsep_or_term ~region state "T_Cart" print_type_expr value.inside

(* Functional types *)

and print_T_Fun state (node : fun_type) =
  let Region.{value; region} = node in
  let fun_type_params, _, codomain = value in
  let children = Tree.[
    mk_child print_fun_type_params fun_type_params;
    mk_child print_codomain        codomain]
  in Tree.make state "T_Fun" ~region children

and print_fun_type_params state (node: fun_type_params) =
  let Region.{value; region} = node in
  Tree.of_sep_or_term ~region state "<parameters>"
                      print_fun_type_param value.inside

and print_fun_type_param state (node: fun_type_param reg) =
  let Region.{region; value} = node in
  let parameter, type_annotation = value in
  let children = Tree.[
    mk_child make_literal          parameter;
    mk_child print_type_annotation type_annotation]
  in Tree.make ~region state "<parameter>" children

and print_codomain state (node: type_expr) =
  Tree.make_unary state "<codomain>" print_type_expr node

(* The integer type *)

and print_T_Int state (node : int_literal) =
  Tree.make_int "T_Int" state node

(* Module paths in type expressions *)

and print_T_ModPath state (node : type_expr module_path reg) =
  print_module_path print_type_expr "T_ModPath" state node

(* Parenthesised type expressions *)

and print_T_Par state (node : type_expr par) =
  Tree.make_unary state "T_Par" print_type_expr node.value.inside

(* Type parameter *)

and print_T_Parameter state (node : parameter_of_type reg) =
  let {kwd_parameter_of=_; module_path} = node.value in
  print_module_path Tree.make_literal "T_Parameter" state module_path

and print_T_Record state (node : type_expr record) =
  print_record print_type_expr "T_Record" state node

and print_record :
  'a.'a Tree.printer -> Tree.root -> Tree.state -> 'a record -> unit =
  fun print root state {value; region} ->
    Tree.of_sep_or_term ~region state root (print_field print) value.inside

and print_field :
  'a.'a Tree.printer -> Tree.state -> 'a field reg -> unit =
  fun print_rhs state field ->
    let Region.{value; region} = field in
    let {attributes; field_id; field_rhs} = value in
    let print_rhs state = print_rhs state <@ snd in
    let children = Tree.[
      mk_child     print_field_id field_id;
      mk_child_opt print_rhs      field_rhs]
    @ mk_children_attr attributes
    in Tree.make ~region state "<field>" children

and print_field_id state = function
  F_Name i -> print_F_Name state i
| F_Num  i -> print_F_Num  state i
| F_Str  i -> print_F_Str  state i

and print_F_Name state (node: field_name) =
  Tree.(make_unary state "F_Name" make_literal node)

and print_F_Num state (node: int_literal) =
  Tree.make_int "F_Num" state node

and print_F_Str state (node: string_literal) =
  Tree.make_string "F_Str" state node

(* Type string *)

and print_T_String state (node: string_literal) =
  Tree.make_string "T_String" state node

(* Discriminated unions *)

and print_T_Union state (node: union_type) =
  let Region.{region; value} = node
  and print = print_record print_type_expr "<subset>" in
  Tree.of_nsep_or_pref ~region state "T_Union" print value

(* Type variable *)

and print_T_Var state (node : variable) =
  Tree.(make_unary state "T_Var" make_literal node)

(* Variant types *)

and print_T_Variant state (node : variant_type) =
  let Region.{value; region} = node in
  Tree.of_nsep_or_pref ~region state "T_Variant" print_variant value

and print_variant state (node : variant reg) =
  let Region.{value; region} = node in
  let {attributes; tuple} = value in
  let {ctor; ctor_params} = tuple.value.inside in
  let children = Tree.(
      mk_child make_literal ctor
   :: [mk_child print_ctor_params ctor_params]
    @ mk_children_attr attributes)
  in Tree.make ~region state ctor#payload children

and print_ctor_params state = function
  None       -> ()
| Some (_,s) -> Tree.of_nsep_or_term state "<parameters>" print_type_expr s

(* PATTERNS *)

and print_pattern state = function
  P_Attr     p -> print_P_Attr     state p
| P_Bytes    p -> print_P_Bytes    state p
| P_Ctor     p -> print_P_Ctor     state p
| P_Int      p -> print_P_Int      state p
| P_Mutez    p -> print_P_Mutez    state p
| P_Nat      p -> print_P_Nat      state p
| P_Record   p -> print_P_Record   state p
| P_String   p -> print_P_String   state p
| P_Tuple    p -> print_P_Tuple    state p
| P_Var      p -> print_P_Var      state p
| P_Verbatim p -> print_P_Verbatim state p

(* Attributes patterns *)

and print_P_Attr state (node : attribute * pattern) =
  let attribute, pattern = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_pattern   pattern]
  in Tree.make state "P_Attr" children

(* Bytes as literals in patterns *)

and print_P_Bytes state (node: (lexeme * Hex.t) wrap) =
  Tree.make_bytes "P_Bytes" state node

(* Data constructors in patterns *)

and print_P_Ctor state (node: ctor) =
  let region = node#region in
  Tree.(make_unary ~region state "P_Ctor" make_literal node)

(* Integers in patterns *)

and print_P_Int state (node : (lexeme * Z.t) wrap) =
  Tree.make_int "P_Int" state node

(* Mutez in patterns *)

and print_P_Mutez state (node : (lexeme * Int64.t) wrap) =
  Tree.make_mutez "P_Mutez" state node

(* Natural numbers in patterns *)

and print_P_Nat state (node : (lexeme * Z.t) wrap) =
  Tree.make_int "P_Nat" state node

(* Record patterns *)

and print_P_Record state (node: pattern record) =
  let Region.{value; region} = node in
  let print = print_field print_pattern in
  Tree.of_sep_or_term ~region state "P_Record" print value.inside

(* String literals as patterns *)

and print_P_String state (node : lexeme wrap) =
  Tree.make_string "P_String" state node

(* Tuple patterns *)

and print_P_Tuple state (node: pattern tuple) =
  print_tuple print_pattern "P_Tuple" state node

and print_tuple :
  'a.'a Tree.printer -> Tree.root -> Tree.state -> 'a tuple -> unit =
  fun print root state brackets ->
    let Region.{region; value} = brackets in
    let components = value.inside in
    Tree.of_sep_or_term ~region state root (print_component print) components

and print_component :
  'a.'a Tree.printer -> Tree.state -> 'a component -> unit =
  fun print state -> function
    None, component -> print state component
  | Some _ellipsis, component -> Tree.make_unary state "..." print component

(* A pattern variable *)

and print_P_Var state (node : variable) =
  Tree.(make_unary state "P_Var" make_literal node)

(* A verbatim string as a pattern *)

and print_P_Verbatim state (node : lexeme wrap) =
  Tree.make_verbatim "P_Verbatim" state node

(* EXPRESSIONS *)

and print_expr state = function
  E_Add      e -> print_E_Add      state e
| E_AddEq    e -> print_E_AddEq    state e
| E_And      e -> print_E_And      state e
| E_App      e -> print_E_App      state e
| E_Assign   e -> print_E_Assign   state e
| E_Attr     e -> print_E_Attr     state e
| E_Bytes    e -> print_E_Bytes    state e
| E_CodeInj  e -> print_E_CodeInj  state e
| E_Contract e -> print_E_Contract state e
| E_Ctor     e -> print_E_Ctor     state e
| E_Div      e -> print_E_Div      state e
| E_DivEq    e -> print_E_DivEq    state e
| E_Equal    e -> print_E_Equal    state e
| E_Fun      e -> print_E_Fun      state e
| E_Geq      e -> print_E_Geq      state e
| E_Gt       e -> print_E_Gt       state e
| E_Int      e -> print_E_Int      state e
| E_Leq      e -> print_E_Leq      state e
| E_Lt       e -> print_E_Lt       state e
| E_MinusEq  e -> print_E_MinusEq  state e
| E_Mod      e -> print_E_Mod      state e
| E_ModEq    e -> print_E_ModEq    state e
| E_ModPath  e -> print_E_ModPath  state e
| E_Mult     e -> print_E_Mult     state e
| E_Mutez    e -> print_E_Mutez    state e
| E_Nat      e -> print_E_Nat      state e
| E_Neg      e -> print_E_Neg      state e
| E_Neq      e -> print_E_Neq      state e
| E_Not      e -> print_E_Not      state e
| E_Or       e -> print_E_Or       state e
| E_Par      e -> print_E_Par      state e
| E_PostDecr e -> print_E_PostDecr state e
| E_PostIncr e -> print_E_PostIncr state e
| E_PreDecr  e -> print_E_PreDecr  state e
| E_PreIncr  e -> print_E_PreIncr  state e
| E_Proj     e -> print_E_Proj     state e
| E_Record   e -> print_E_Record   state e
| E_String   e -> print_E_String   state e
| E_Sub      e -> print_E_Sub      state e
| E_Ternary  e -> print_E_Ternary  state e
| E_TimesEq  e -> print_E_TimesEq  state e
| E_Tuple    e -> print_E_Tuple    state e
| E_Typed    e -> print_E_Typed    state e
| E_Update   e -> print_E_Update   state e
| E_Var      e -> print_E_Var      state e
| E_Verbatim e -> print_E_Verbatim state e

(* Arithmetic addition *)

and print_E_Add state (node : plus bin_op reg) =
  print_bin_op state "E_Add" node

and print_bin_op state root (node : 'op bin_op reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child print_expr value.arg1;
    mk_child print_expr value.arg2]
  in Tree.make state root ~region children

(* Addition & assignment *)

and print_E_AddEq state (node: plus_eq bin_op reg) =
  print_bin_op state "E_AddPlus" node

(* Boolean conjunction *)

and print_E_And state (node : bool_and bin_op reg) =
  print_bin_op state "E_And" node

(* Data constructor application or function call *)

and print_E_App state (node : (expr * arguments) reg) =
  let Region.{value; region} = node in
  let fun_or_ctor, args = value
  and mk_func state =
    Tree.make_unary state "<fun or ctor>" print_expr
  and mk_args state (node : arguments) =
    Tree.of_sepseq state "<arguments>" print_expr node.value.inside
  in
  let children = Tree.[
    mk_child mk_func fun_or_ctor;
    mk_child mk_args args]
  in Tree.make state "E_App" ~region children

(* Assignment *)

and print_E_Assign state (node: equal bin_op reg) =
  print_bin_op state "E_Assign" node

(* Attributed expressions *)

and print_E_Attr state (node : attribute * expr) =
  let attribute, expr = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_expr      expr]
  in Tree.make state "E_Attr" children

(* Bytes as expressions *)

and print_E_Bytes state (node : (lexeme * Hex.t) wrap) =
  Tree.make_bytes "E_Bytes" state node

(* Code Injection *)

and print_E_CodeInj state (node : code_inj reg) =
  let Region.{value; region} = node in
  let {language; code} = value in
  let children = Tree.[
    mk_child print_language language;
    mk_child print_code     code]
  in Tree.make state "E_CodeInj" ~region children

and print_language state (node : language) =
  Tree.(make_unary state "<language>" make_literal node)

and print_code state (node : expr) =
  Tree.make_unary state "<code>" print_expr node

(* Contract of expression *)

and print_E_Contract state (node: contract_of_expr reg) =
  let {kwd_contract_of=_; module_path} = node.value in
  let path = module_path.value.inside in
  print_module_path Tree.make_literal "E_Contract" state path

(* Data constructor as expressions *)

and print_E_Ctor state (node : ctor) =
  let region = node#region in
  Tree.(make_unary ~region state "E_Ctor" make_literal node)

(* The Euclidean quotient *)

and print_E_Div state (node : slash bin_op reg) =
  print_bin_op state "E_Div" node

(* Euclidean quotient & assignment *)

and print_E_DivEq state (node: div_eq bin_op reg) =
  print_bin_op state "E_DivEq" node

(* Equality *)

and print_E_Equal state (node : equal_cmp bin_op reg) =
  print_bin_op state "E_Equal" node

(* Functional expressions *)

and print_E_Fun state (node : fun_expr reg) =
  let Region.{value; region} = node in
  let {type_vars; parameters; rhs_type; arrow=_; fun_body} = value in
  let children = Tree.[
    mk_child_opt print_type_vars       type_vars;
    mk_child     print_parameters      parameters;
    mk_child_opt print_type_annotation rhs_type;
    mk_child     print_fun_body        fun_body]
  in Tree.make ~region state "E_Fun" children

and print_parameters state = function
  Params   p -> print_Params   state p
| OneParam p -> print_OneParam state p

and print_Params state (node: (pattern, comma) sep_or_term par) =
  let Region.{value; region} = node in
  let params = value.inside in
  Tree.of_sep_or_term ~region state "Params" print_pattern params

and print_OneParam state (node: variable) =
  Tree.(make_unary state "OneParam" make_literal node)

and print_fun_body state = function
  FunBody  b -> print_FunBody  state b
| ExprBody b -> print_ExprBody state b

and print_FunBody state (node: statements braces) =
  Tree.of_nseq state "FunBody"
               (fun state -> print_statement state <@ fst) node.value.inside

and print_ExprBody state (node: expr) =
  Tree.make_unary state "ExprBody" print_expr node

(* Greater or Equal *)

and print_E_Geq state (node : geq bin_op reg) =
  print_bin_op state "E_Geq" node

(* Greater Than *)

and print_E_Gt state (node : gt bin_op reg) =
  print_bin_op state "E_Gt" node

(* Integer literals as expressions *)

and print_E_Int state (node : (lexeme * Z.t) wrap) =
  Tree.make_int "E_Int" state node

(* Lower or Equal *)

and print_E_Leq state (node : leq bin_op reg) =
  print_bin_op state "E_Leq" node

(* Lower Than *)

and print_E_Lt state (node : lt bin_op reg) =
  print_bin_op state "E_lt" node

(* Subtraction & assignment *)

and print_E_MinusEq state (node: minus_eq bin_op reg) =
  print_bin_op state "E_MinusEq" node

(* Arithmetic modulo *)

and print_E_Mod state (node : modulo bin_op reg) =
  print_bin_op state "E_Mod" node

(* Arithmetic module & assignment *)

and print_E_ModEq state (node: mod_eq bin_op reg) =
  print_bin_op state "E_ModEq" node

(* Qualified expression *)

and print_E_ModPath state (node : expr module_path reg) =
  print_module_path print_expr "E_ModPath" state node

(* Multiplication *)

and print_E_Mult state (node : times bin_op reg) =
  print_bin_op state "E_Mult" node

(* Mutez literals *)

and print_E_Mutez state (node : (lexeme * Int64.t) wrap) =
  Tree.make_mutez "E_Mutez" state node

(* Natural numbers *)

and print_E_Nat state (node : (lexeme * Z.t) wrap) =
  Tree.make_nat "E_Nat" state node

(* Arithmetic negation *)

and print_E_Neg state (node : minus un_op reg) =
  print_un_op state "E_Neg" node

and print_un_op state root (node : 'op un_op reg) =
  let Region.{value; region} = node in
  Tree.make_unary state root ~region print_expr value.arg

(* Inequality *)

and print_E_Neq state (node : neq bin_op reg) =
  print_bin_op state "E_Neq" node

(* Logical negation *)

and print_E_Not state (node : negation un_op reg) =
  print_un_op state "E_Not" node

(* Logical disjunction *)

and print_E_Or state (node : bool_or bin_op reg) =
  print_bin_op state "E_Or" node

(* Parenthesised expressions *)

and print_E_Par state (node : expr par) =
  let Region.{value; region} = node in
  Tree.make_unary state "E_Par" ~region print_expr value.inside

(* Increments & decrements *)

and print_E_PostDecr state (node : decrement un_op reg) =
  print_un_op state "E_PostDecr" node

and print_E_PostIncr state (node: increment un_op reg) =
  print_un_op state "E_PostIncr" node

and print_E_PreDecr state (node: decrement un_op reg) =
  print_un_op state "E_PreDecr" node

and print_E_PreIncr state (node: increment un_op reg) =
  print_un_op state "E_PreIncr" node

(* Projections *)

and print_E_Proj state (node : projection reg) =
  let Region.{value; region} = node in
  let {record_or_tuple; field_path} = value in
  let children = Tree.(
       mk_child         print_expr      record_or_tuple
    :: mk_children_nseq print_selection field_path)
  in Tree.make state ~region "E_Proj" children

and print_selection state = function
  FieldName s -> print_FieldName state s
| FieldStr  s -> print_FieldStr  state s
| Component s -> print_Component state s

and print_FieldName state (node : dot * field_name) =
  Tree.(make_unary state "FieldName" make_literal (snd node))

and print_FieldStr state (node : dot * string_literal) =
  Tree.make_string "FieldStr" state (snd node)

and print_Component state (node : int_literal brackets) =
  Tree.(make_int "Component" state node.value.inside)

(* Record expressions *)

and print_E_Record state (node: expr record) =
  print_record print_expr "E_Record" state node

and print_E_String state (node: lexeme wrap) =
  Tree.make_string "E_String" state node

and print_E_Sub state (node: minus bin_op reg) =
  print_bin_op state "E_Sub" node

and print_E_Ternary state (node: ternary reg) =
  let Region.{value; region} = node in
  let {condition; qmark=_; truthy; colon=_; falsy} = value in
  let print_truthy state (node: expr) =
    Tree.make_unary state "<true>" print_expr node
  and print_falsy state (node: expr) =
    Tree.make_unary state "<false>" print_expr node in
  let children = Tree.[
    mk_child print_expr   condition;
    mk_child print_truthy truthy;
    mk_child print_falsy  falsy]
  in Tree.make state ~region "E_Ternary" children

and print_E_TimesEq state (node: times_eq bin_op reg) =
  print_bin_op state "E_TimesEq" node

and print_E_Tuple state (node: expr tuple) =
  print_tuple print_expr "E_Tuple" state node

and print_E_Typed state (node: typed_expr reg) =
  let Region.{value; region} = node in
  let expr, _, type_expr = value in
  let children = Tree.[
    mk_child print_expr      expr;
    mk_child print_type_expr type_expr]
  in Tree.make state ~region "E_Typed" children

and print_E_Update state (node: update_expr braces) =
  let Region.{region; value} = node in
  let {ellipsis=_; record; sep=_; updates} = value.inside in
  let print_updates state (node: (expr field reg, semi) nsep_or_term) =
    let print = print_field print_expr in
    Tree.of_nsep_or_term state "<updates>" print node in
  let children = Tree.[
    mk_child print_expr    record;
    mk_child print_updates updates]
  in Tree.make state ~region "E_Update" children

and print_E_Var state (node: variable) =
  Tree.(make_unary state "E_Var" make_literal node)

and print_E_Verbatim state (node: lexeme wrap) =
  Tree.make_verbatim "E_Verbatim" state node

(* STATEMENTS *)

and print_statement state = function
  S_Attr   s -> print_S_Attr   state s
| S_Block  s -> print_S_Block  state s
| S_Break  s -> print_S_Break  state s
| S_Cond   s -> print_S_Cond   state s
| S_Decl   s -> print_S_Decl   state s
| S_Expr   s -> print_S_Expr   state s
| S_For    s -> print_S_For    state s
| S_ForOf  s -> print_S_ForOf  state s
| S_Return s -> print_S_Return state s
| S_Switch s -> print_S_Switch state s
| S_While  s -> print_S_While  state s

(* Attributed statement *)

and print_S_Attr state (node: attribute * statement) =
  let attribute, stmt = node in
  let children = Tree.[
    mk_child print_attribute attribute;
    mk_child print_statement stmt]
  in Tree.make state "S_Attr" children

(* Block of statements *)

and print_S_Block state (node: statements braces) =
  let Region.{region; value} = node in
  let stmts = value.inside in
  Tree.of_nseq ~region state "S_Block"
               (fun state -> print_statement state <@ fst) stmts

(* Break statement *)

and print_S_Break state (node: kwd_break) =
  Tree.make_node ~region:node#region state "S_Break"

(* Conditional statement *)

and print_S_Cond state (node: cond_stmt reg) =
  let Region.{value; region} = node in
  let {kwd_if=_; test; if_so; if_not} = value in

  let print_if_so state (node: statement) =
    Tree.make_unary state "<true>" print_statement node

  and print_if_not state (node: kwd_else * statement) =
    Tree.make_unary state "<false>" print_statement (snd node) in

  let children = Tree.[
    mk_child     print_expr   test.value.inside;
    mk_child     print_if_so  if_so;
    mk_child_opt print_if_not if_not]
  in Tree.make ~region state "S_Cond" children

(* Declaration as a statement *)

and print_S_Decl state (node: declaration) =
  Tree.make_unary state "S_Decl" print_declaration node

(* Expression as a statement *)

and print_S_Expr state (node: expr) =
  Tree.make_unary state "S_Expr" print_expr node

(* For-loop statement *)

and print_S_For state (node: for_stmt reg) =
  let Region.{value; region} = node in
  let {kwd_for=_; range; for_body} : for_stmt = value in
  let range = range.value.inside in
  let {initialiser; semi1=_; condition; semi2=_; afterthought} = range in
  let print_initialiser state = Tree.make_unary state "<init>" print_statement
  and print_cond state = Tree.make_unary state "<cond>" print_expr in
  let open Tree in
  let children = [
    mk_child_opt print_initialiser initialiser;
    mk_child_opt print_cond        condition]
  @ mk_children_nsepseq_opt ~root:"<afterthought>" print_expr afterthought
  @ [mk_child print_loop_body for_body]
  in make state ~region "S_For" children

and print_loop_body state = Tree.make_unary state "<body>" print_statement

(* ForOf-loop statement *)

and print_S_ForOf state (node: for_of_stmt reg) =
  let Region.{value; region} = node in
  let {kwd_for=_; range; for_of_body} = value in
  let range = range.value.inside in
  let {index_kind; index; kwd_of=_; expr} = range in
  let print_index state = function
    `Let _,   var -> Tree.(make_unary state "let"   make_literal var)
  | `Const _, var -> Tree.(make_unary state "const" make_literal var) in
  let children = Tree.[
    mk_child print_index     (index_kind, index);
    mk_child print_expr      expr;
    mk_child print_loop_body for_of_body]
  in Tree.make ~region state "S_ForOf" children

(* return statement *)

and print_S_Return state (node: return_stmt reg) =
  Tree.make_node ~region:node.region state "S_Return"

(* Switch statement *)

and print_S_Switch state (node: switch_stmt reg) =
  let Region.{region; value} = node in
  let {kwd_switch=_; subject; cases} = value in
  let subject = subject.value.inside
  and cases   = cases.value.inside in
  let print_subject state (node: expr) =
    Tree.make_unary state "<subject>" print_expr node in
  let children = Tree.[
    mk_child print_subject subject;
    mk_child print_cases   cases]
  in Tree.make state ~region "S_Switch" children

and print_cases state = function
  AllCases c -> print_AllCases state c
| Default  c -> print_Default  state c

and print_AllCases state (node: all_cases) =
  let normal_cases, default_case_opt = node in
  let children = Tree.(
    mk_children_nseq print_case normal_cases
    @ [mk_child_opt print_switch_default default_case_opt])
  in Tree.make state "AllCases" children

and print_case state (node: switch_case reg) =
  let Region.{value; region} = node in
  let {kwd_case=_; expr; colon=_; case_body} = value in
  let children = Tree.[
    mk_child     print_expr       expr;
    mk_child_opt print_statements case_body]
  in Tree.make ~region state "<case>" children

and print_Default state (node: switch_default reg) =
  print_switch_default state node

and print_switch_default state (node: switch_default reg) =
  let Region.{value; region} = node in
  let {kwd_default=_; colon=_; default_body} = value in
  match default_body with
    None -> Tree.make_node ~region state "<empty default>"
  | Some stmts ->
      Tree.make_unary ~region state "Default" print_statements stmts

and print_statements state (node: statements) =
  Tree.of_nseq state "<statements>"
               (fun state -> print_statement state <@ fst) node

(* While-loop statement *)

and print_S_While state (node: while_stmt reg) =
  let Region.{region; value} = node in
  let {kwd_while=_; invariant; while_body} = value in
  let children = Tree.[
    mk_child print_expr      invariant.value.inside;
    mk_child print_statement while_body]
  in Tree.make state ~region "S_While" children

(* PRINTING (client-slide) *)

type ('src, 'dst) printer = Tree.state -> 'src -> 'dst

let print_to_buffer state cst =
  print_cst state cst; Tree.to_buffer state

let print_to_string state cst =
  Buffer.contents (print_to_buffer state cst)

let print_pattern_to_string state pattern =
  print_pattern state pattern;
  Buffer.contents (Tree.to_buffer state)

(* Aliases *)

let to_buffer = print_to_buffer
let to_string = print_to_string
let pattern_to_string = print_pattern_to_string
