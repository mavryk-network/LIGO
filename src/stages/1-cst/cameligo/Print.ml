(* PRINTING THE CST *)

(* This module produces an arborescent, textual representation of a
   subset of the Concrete Abstract Tree (CST). It aims at a readable
   format with the most relevant nodes, with source locations. This
   functionality is most useful when testing the parser, for example,
   checking that a particular node corresponding to an operator has
   the expected associativity with the same kind, or the expected
   priority over another. *)

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

type ('a, 'sep) nsepseq = ('a, 'sep) Utils.nsepseq
type 'a nseq = 'a Utils.nseq

let print_attribute state (node : Attr.t reg) =
  let key, val_opt = node.value in
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

let mk_children_attr (node : Attr.t reg list) =
  Tree.mk_children_list print_attribute ~root:"<attributes>" node

(* Preprocessing directives *)

let print_Directive state (node : Directive.t) =
  let region, string = Directive.project node in
  Tree.(make_unary state "Directive" make_node ~region string)

(* PRINTING THE CST *)

let rec print_cst state (node: cst) =
  Tree.of_nseq state "<cst>" print_declaration node.decl

(* DECLARATIONS *)

and print_declaration state = function
  Let         d -> print_Let         state d
| TypeDecl    d -> print_TypeDecl    state d
| ModuleDecl  d -> print_ModuleDecl  state d
| ModuleAlias d -> print_ModuleAlias state d
| Directive   d -> print_Directive   state d

(* Value declarations *)

and print_Let state (node: let_decl reg) =
  let Region.{region; value} = node in
  let _, kwd_rec_opt, let_binding, attributes = value in
  let children = Tree.(mk_child_opt make_literal kwd_rec_opt)
                 :: mk_children_binding let_binding
                 @ mk_children_attr attributes
  in Tree.make ~region state "Let" children

and mk_children_binding (node : let_binding) =
  let {type_params; binders; rhs_type; let_rhs; _} = node in
  Tree.[mk_child_opt print_type_params     type_params;
        mk_child     print_binders         binders;
        mk_child_opt print_type_annotation rhs_type;
        mk_child     print_expr            let_rhs]

and print_type_params state (node: type_params par reg) =
  let Region.{region; value} = node in
  Tree.(of_nseq state ~region "<type parameters>" make_literal
          value.inside.type_vars)

and print_binders state (node: pattern nseq) =
  Tree.of_nseq state "<binders>" print_pattern node

and print_type_annotation state (_, type_expr) =
  Tree.make_unary state "<type>" print_type_expr type_expr

(* Type declaration *)

and print_TypeDecl state (node: type_decl reg) =
  let Region.{value; region} = node in
  Tree.make ~region state "TypeDecl" @@ mk_children_type_decl value

and mk_children_type_decl (node: type_decl) =
  let {name; params; type_expr; _} = node in

  let print_QParam state (node: type_var reg) =
    Tree.make_unary state "QParam" print_type_var node

  and print_QParamTuple state (node: (type_var reg, comma) nsepseq par reg) =
    let Region.{value; region} = node in
    Tree.of_nsepseq state ~region "QParamTuple"
      print_type_var value.inside in

  let print_type_vars state = function
    QParam      tv -> print_QParam      state tv
  | QParamTuple tv -> print_QParamTuple state tv

  in Tree.[mk_child     make_literal    name;
           mk_child_opt print_type_vars params;
           mk_child     print_type_expr type_expr]

and print_type_var state (node: type_var reg) =
  Tree.make_literal state node.value.name (* We don't print the backquote *)

(* Module declaration *)

and print_ModuleDecl state (node: module_decl reg) =
  let Region.{value; region} = node in
  let children = mk_children_module_decl value
  in Tree.make state ~region "ModuleDecl" children

and mk_children_module_decl (node: module_decl) =
  Tree.[mk_child make_literal node.name;
        mk_child print_cst    node.module_]

(* Module aliases *)

and print_ModuleAlias state (node: module_alias reg) =
  let Region.{value; region} = node in
  let children = mk_children_module_alias value
  in Tree.make state ~region "ModuleAlias" children

and mk_children_module_alias (node: module_alias) =
  let print_path state (node: (module_name, dot) nsepseq) =
    Tree.(of_nsepseq state "<path>" make_literal node)
  in Tree.[mk_child make_literal node.alias;
           mk_child print_path   node.binders]

(* PATTERNS *)

and print_pattern state = function
  PConstr   p -> print_PConstr   state p
| PVar      p -> print_PVar      state p
| PInt      p -> print_PInt      state p
| PNat      p -> print_PNat      state p
| PBytes    p -> print_PBytes    state p
| PString   p -> print_PString   state p
| PVerbatim p -> print_PVerbatim state p
| PUnit     p -> print_PUnit     state p
| PList     p -> print_PList     state p
| PTuple    p -> print_PTuple    state p
| PPar      p -> print_PPar      state p
| PRecord   p -> print_PRecord   state p
| PTyped    p -> print_PTyped    state p

(* Application of a data constructor as a pattern *)

and print_PConstr state (node: (constr * pattern option) reg) =
  let Region.{region; value} = node in
  let ctor, args_opt = value in
  let children = Tree.[
    mk_child     make_literal ctor;
    mk_child_opt print_pattern args_opt]
  in Tree.make state ~region "PConstr" children

(* Pattern variable *)

and print_PVar state (node: var_pattern reg) =
  let Region.{region; value} = node in
  let children = mk_children_var_pattern value
  in Tree.make state ~region "PVar" children

and mk_children_var_pattern (node: var_pattern) =
  Tree.(mk_child make_literal node.variable)
  :: mk_children_attr node.attributes

(* Integer in patterns *)

and print_PInt state (node : (lexeme * Z.t) wrap) =
  Tree.make_int "PInt" state node

(* Natural numbers in patterns *)

and print_PNat state (node : (lexeme * Z.t) wrap) =
  Tree.make_int "PNat" state node

(* Bytes as literals in patterns *)

and print_PBytes state (node : (lexeme * Hex.t) wrap) =
  Tree.make_bytes "PBytes" state node

(* String literals as patterns *)

and print_PString state (node : lexeme wrap) =
  Tree.(make_unary state "PString" make_string node)

(* A verbatim string in patterns *)

and print_PVerbatim state (node : lexeme wrap) =
  Tree.(make_unary state "PVerbatim" make_string node)

(* Mutez in patterns *) (* TODO: Missing in the CST *)
(*
and print_PMutez state (node : (lexeme * Int64.t) wrap) =
  Tree.make_mutez "PMutez" state node
*)

(* Unit pattern *)

and print_PUnit state (node: the_unit reg) =
  Tree.make_node state ~region:node.region "()"

(* List patterns *)

and print_PList state = function
  PListComp p -> print_PListComp state p
| PCons     p -> print_PCons     state p

and print_PListComp state (node: pattern injection reg) =
  let Region.{region; value} = node in
  Tree.of_sepseq ~region state "PListComp" print_pattern value.elements

and print_PCons state (node: (pattern * cons * pattern) reg) =
  let Region.{region; value} = node in
  let hd_pattern, _, tl_pattern = value in
  let children = Tree.[
    mk_child print_pattern hd_pattern;
    mk_child print_pattern tl_pattern]
  in Tree.make state ~region "PCons" children

(* Tuple patterns *)

and print_PTuple state (node: (pattern, comma) nsepseq reg) =
  let Region.{region; value} = node in
  Tree.of_nsepseq ~region state "PTuple" print_pattern value

(* Parenthesised patterns *)

and print_PPar state (node: pattern par reg) =
  let Region.{region; value} = node in
  Tree.make_unary state ~region "PPar" print_pattern value.inside

(* Record patterns *)

and print_PRecord state (node: field_pattern reg ne_injection reg) =
  let Region.{region; value} = node in
  let children =
    Tree.mk_children_nsepseq print_field_pattern value.ne_elements
    @ mk_children_attr value.attributes in
  Tree.make state ~region "PRecord" children

and print_field_pattern state (node: field_pattern reg) =
  let Region.{region; value} = node in
  let children = Tree.[
    mk_child make_literal  value.field_name;
    mk_child print_pattern value.pattern] in
  Tree.make state ~region "<field>" children

(* Typed patterns *)

and print_PTyped state (node: typed_pattern reg) =
  let Region.{region; value} = node in
  let children = Tree.[
    mk_child print_pattern   value.pattern;
    mk_child print_type_expr value.type_expr]
  in Tree.make state ~region "PTyped" children

(* EXPRESSIONS *)

and print_expr state = function
  ECase     e -> print_ECase     state e
| ECond     e -> print_ECond     state e
| EAnnot    e -> print_EAnnot    state e
| ELogic    e -> print_ELogic    state e
| EArith    e -> print_EArith    state e
| EString   e -> print_EString   state e
| EList     e -> print_EList     state e
| EConstr   e -> print_EConstr   state e
| ERecord   e -> print_ERecord   state e
| EProj     e -> print_EProj     state e
| EModA     e -> print_EModA     state e
| EUpdate   e -> print_EUpdate   state e
| EVar      e -> print_EVar      state e
| ECall     e -> print_ECall     state e
| EBytes    e -> print_EBytes    state e
| EUnit     e -> print_EUnit     state e
| ETuple    e -> print_ETuple    state e
| EPar      e -> print_EPar      state e
| ELetIn    e -> print_ELetIn    state e
| ELetMutIn e -> print_ELetMutIn state e
| ETypeIn   e -> print_ETypeIn   state e
| EModIn    e -> print_EModIn    state e
| EModAlias e -> print_EModAlias state e
| EFun      e -> print_EFun      state e
| ESeq      e -> print_ESeq      state e
| ECodeInj  e -> print_ECodeInj  state e
| ERevApp   e -> print_ERevApp   state e
| EAssign   e -> print_EAssign   state e
| EFor      e -> print_EFor      state e
| EForIn    e -> print_EForIn    state e
| EWhile    e -> print_EWhile    state e

(* Pattern matching *)

and print_ECase state (node: case reg) =
  let Region.{region; value} = node in
  let {expr; cases; _} = value in

  let print_subject state (node: expr) =
    Tree.make_unary state "<subject>" print_expr node in

  let print_case_clause state (node: case_clause reg) =
    let Region.{region; value} = node in

    let children = Tree.[
      mk_child print_pattern value.pattern;
      mk_child print_expr    value.rhs]
    in Tree.make state ~region "<match clause>" children in

  let children = Tree.(
     mk_child print_subject expr
  :: mk_children_nsepseq print_case_clause cases.value)
  in Tree.make state ~region "ECase" children

(* Conditional expressions *)

and print_ECond state (node: cond_expr reg) =
  let Region.{region; value} = node in

  let print_test state (node: expr) =
    Tree.make_unary state "<test>" print_expr node

  and print_ifso state (node: expr) =
    Tree.make_unary state "<ifso>" print_expr node

  and print_ifnot state (node: kwd_else * expr) =
    Tree.make_unary state "<ifnot>" print_expr (snd node) in

  let children = Tree.[
    mk_child print_test      value.test;
    mk_child print_ifso      value.ifso;
    mk_child_opt print_ifnot value.ifnot]
  in Tree.make state ~region "ECond" children

(* Expressions annotated by a type *)

and print_EAnnot state (node: annot_expr par reg) =
  let Region.{region; value} = node in
  let expr, _, type_expr = value.inside in
  let children = Tree.[
    mk_child print_expr      expr;
    mk_child print_type_expr type_expr]
  in Tree.make state ~region "EAnnot" children

(* Boolean expressions and comparisons  *)

and print_ELogic state = function
  BoolExpr e -> print_BoolExpr state e
| CompExpr e -> print_CompExpr state e

and print_BoolExpr state = function
  Or  e -> print_Or  state e
| And e -> print_And state e
| Not e -> print_Not state e

and print_Or state (node: kwd_or bin_op reg) =
  print_bin_op state "Or" node

and print_And state (node: kwd_and bin_op reg) =
  print_bin_op state "And" node

and print_Not state (node: kwd_not un_op reg) =
  print_un_op state "Not" node

and print_un_op state root (node : 'op un_op reg) =
  let Region.{value; region} = node in
  Tree.make_unary state root ~region print_expr value.arg

and print_bin_op state root (node : 'op bin_op reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child print_expr value.arg1;
    mk_child print_expr value.arg2]
  in Tree.make state root ~region children

and print_CompExpr state = function
  Lt    e -> print_Lt    state e
| Leq   e -> print_Leq   state e
| Gt    e -> print_Gt    state e
| Geq   e -> print_Geq   state e
| Equal e -> print_Equal state e
| Neq   e -> print_Neq   state e

and print_Lt state (node: lt bin_op reg) =
  print_bin_op state "Lt" node

and print_Leq state (node: leq bin_op reg) =
  print_bin_op state "Leq" node

and print_Gt state (node: gt bin_op reg) =
  print_bin_op state "Gt" node

and print_Geq state (node: geq bin_op reg) =
  print_bin_op state "Geq" node

and print_Equal state (node: equal bin_op reg) =
  print_bin_op state "Equal" node

and print_Neq state (node: neq bin_op reg) =
  print_bin_op state "Neq" node

(* Arithmetic expressions *)

and print_EArith state = function
  Add   e -> print_Add   state e
| Sub   e -> print_Sub   state e
| Mult  e -> print_Mult  state e
| Div   e -> print_Div   state e
| Mod   e -> print_Mod   state e
| Land  e -> print_Land  state e
| Lor   e -> print_Lor   state e
| Lxor  e -> print_Lxor  state e
| Lsl   e -> print_Lsl   state e
| Lsr   e -> print_Lsr   state e
| Neg   e -> print_Neg   state e
| Int   e -> print_Int   state e
| Nat   e -> print_Nat   state e
| Mutez e -> print_Mutez state e

and print_Add state (node: plus bin_op reg) =
  print_bin_op state "Add" node

and print_Sub state (node: minus bin_op reg) =
  print_bin_op state "Sub" node

and print_Mult state (node: times bin_op reg) =
  print_bin_op state "Mult" node

and print_Div state (node: slash bin_op reg) =
  print_bin_op state "Div" node

and print_Mod state (node: kwd_mod bin_op reg) =
  print_bin_op state "Mod" node

and print_Land state (node: kwd_land bin_op reg) =
  print_bin_op state "Land" node

and print_Lor state (node: kwd_lor bin_op reg) =
  print_bin_op state "Lor" node

and print_Lxor state (node: kwd_lxor bin_op reg) =
  print_bin_op state "Lxor" node

and print_Lsl state (node: kwd_lsl bin_op reg) =
  print_bin_op state "Lsl" node

and print_Lsr state (node: kwd_lsr bin_op reg) =
  print_bin_op state "Lsr" node

and print_Neg state (node: minus un_op reg) =
  print_un_op state "Neg" node

and print_Int state (node: (lexeme * Z.t) wrap) =
  Tree.make_int "Int" state node

and print_Nat state (node: (lexeme * Z.t) wrap) =
  Tree.make_nat "Nat" state node

and print_Mutez state (node: (lexeme * Int64.t) wrap) =
  Tree.make_mutez "Mutez" state node

(* String literals as expressions *)

and print_EString state = function
  String   e -> Tree.(make_unary state "String" make_string e)
| Verbatim e -> Tree.(make_unary state "Verbatim" make_verbatim e)
| Cat      e -> print_bin_op state "Cat" e

(* Lists of expressions defined intensionally *)

and print_EList state = function
  ECons     e -> print_ECons     state e
| EListComp e -> print_EListComp state e

and print_ECons state (node: cons bin_op reg) =
   print_bin_op state "ECons" node

and print_EListComp state (node: expr injection reg) =
  let Region.{region; value} = node in
  Tree.of_sepseq ~region state "EListComp" print_expr value.elements

(* Constructor application (or constant constructor) as expressions *)

and print_EConstr state (node: (constr * expr option) reg) =
  let Region.{region; value} = node in
  let ctor, arg_opt = value in
  let children = Tree.[
    mk_child     make_literal ctor;
    mk_child_opt print_expr   arg_opt]
  in Tree.make ~region state "EConstr" children

(* Record expression *)

and print_ERecord state (node: record reg) =
    let Region.{region; value} = node in

    let print_Property state (node: field_assign_property) =
      let children = Tree.[
        mk_child make_literal node.field_name;
        mk_child print_expr   node.field_expr]
      in Tree.make state ~region "Property" children

    and print_Punned_property state (node: field_name) =
      Tree.(make_unary state "Punned_property" make_literal node) in

    let print_field_assign state (node: field_assign reg) =
      match node.value with
        Property        e -> print_Property        state e
      | Punned_property e -> print_Punned_property state e in

    let children =
      Tree.mk_children_nsepseq print_field_assign value.ne_elements
    @ mk_children_attr value.attributes
    in Tree.make state ~region "ERecord" children

(* Projections *)

and print_EProj state (node: projection reg) =
  print_projection state "EProj" node

and print_projection state root (node: projection reg) =
  let Region.{value; region} = node in
  let children = Tree.(
     mk_child make_literal value.struct_name
  :: mk_children_nsepseq print_selection value.field_path)
  in Tree.make state ~region root children

and print_selection state = function
  FieldName e -> Tree.(make_unary state "FieldName" make_literal e)
| Component e -> Tree.make_int "Component" state e

(* Qualified values (through a module path) *)

and print_EModA state (node: expr module_access reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child make_literal value.module_name;
    mk_child print_expr   value.field]
  in Tree.make state ~region "EModA" children

(* Functional updates of records *)

and print_EUpdate state (node: update reg) =
  let Region.{region; value} = node in
  let {record; updates; _} = value in
  let elements = updates.value.ne_elements in

  let print_Path_property state (node: field_path_assignment_property) =
    let children = Tree.[
      mk_child print_path node.field_path;
      mk_child print_expr node.field_expr]
    in Tree.make state "Path_property" children

  and print_Path_punned_property state (node: field_name) =
    Tree.(make_unary state "Path_punned_property" make_literal node) in

  let print_path_assignment state (node: field_path_assignment reg) =
    match node.value with
      Path_property        e -> print_Path_property        state e
    | Path_punned_property e -> print_Path_punned_property state e in

  let children = Tree.(
     mk_child print_path record
  :: mk_children_nsepseq print_path_assignment elements)
  in Tree.make state ~region "EUpdate" children

and print_path state = function
  Name e -> Tree.(make_unary state "Name" make_literal e)
| Path e -> print_projection state "Path" e

(* Expression variable *)

and print_EVar state (node: variable) =
  Tree.(make_unary state "EVar" make_literal node)

and print_ECall state (node: (expr * expr nseq) reg) =
  let lambda, args = node.value in
  let children = Tree.(
     mk_child print_expr lambda
  :: mk_children_nseq print_expr args)
  in Tree.make state "ECall" children

(* Byte values *)

and print_EBytes state (node: (lexeme * Hex.t) wrap) =
  Tree.make_bytes "EBytes" state node

(* Unit value *)

and print_EUnit state (node: the_unit reg) =
  Tree.make_node state ~region:node.region "EUnit"

(* Tuple expression *)

and print_ETuple state (node: (expr, comma) nsepseq reg) =
  let Region.{region; value} = node in
  Tree.of_nsepseq state ~region "ETuple" print_expr value

(* Parenthesised expression *)

and print_EPar state (node: expr par reg) =
  let Region.{region; value} = node in
  Tree.make_unary state ~region "EPar" print_expr value.inside

(* Local value definition *)

and print_ELetIn state (node: let_in reg) =
  let Region.{region; value} = node in
  let {kwd_rec; binding; body; attributes; _} = value in

  let children = Tree.(mk_child_opt make_literal kwd_rec)
                 :: mk_children_binding binding
                 @  mk_children_attr attributes
                 @  [Tree.mk_child print_body body]
  in Tree.make ~region state "ELetIn" children

and print_body state (node: expr) =
  Tree.make_unary state "<body>" print_expr node

(* Local mutable values *)

and print_ELetMutIn state (node: let_mut_in reg) =
  let Region.{region; value} = node in
  let {kwd_mut; binding; body; attributes; _} = value in

  let children = Tree.(mk_child make_literal kwd_mut)
                 :: mk_children_binding binding
                 @  mk_children_attr attributes
                 @  [Tree.mk_child print_body body]
  in Tree.make ~region state "ELetMutIn" children

(* Local type definition *)

and print_ETypeIn state (node: type_in reg) =
  let Region.{value; region} = node in
  let {type_decl; body; _} = value in
  let children = mk_children_type_decl type_decl
                 @ [Tree.mk_child print_body body]
  in Tree.make ~region state "ETypeIn" children

(* Local module definition *)

and print_EModIn state (node: mod_in reg) =
  let Region.{region; value} = node in
  let {mod_decl; body; _} = value in
  let children = mk_children_module_decl mod_decl
                 @ [Tree.mk_child print_body body]
  in Tree.make state ~region "EModIn" children

(* Module aliases *)

and print_EModAlias state (node: mod_alias reg) =
  let Region.{region; value} = node in
  let children = mk_children_module_alias value.mod_alias
                 @ [Tree.mk_child print_body value.body]
  in Tree.make state ~region "EModAlias" children

(* Functional expressions  *)

and print_EFun state (node: fun_expr reg) =
  let Region.{region; value} = node in
  let {type_params; binders; rhs_type; body; attributes; _} = value in
  let children = Tree.[
    mk_child_opt print_type_params     type_params;
    mk_child     print_binders         binders;
    mk_child_opt print_type_annotation rhs_type;
    mk_child     print_body            body]
  @ mk_children_attr attributes
  in Tree.make state ~region "EFun" children

(* Sequence expressions *)

and print_ESeq state (node: expr injection reg) =
  let Region.{region; value} = node in
  let children =
    Tree.mk_children_sepseq print_expr value.elements
  in Tree.make state ~region "ESeq" children

(* Code injection *)

and print_ECodeInj state (node: code_inj reg) =
  let Region.{value; region} = node in
  Tree.make_unary state ~region "ECodeInj" print_expr value.code

(* Reverse-application operator *)

and print_ERevApp state (node: rev_app bin_op reg) =
  print_bin_op state "ERevApp" node

(* Assignment expression *)

and print_EAssign state (node: assign reg) =
  let Region.{region; value} = node in
  let {binder; expr; _} = value in
  let children = Tree.[
    mk_child make_literal binder;
    mk_child print_expr   expr]
  in Tree.make state ~region "EAssign" children

(* For-loop *)

and print_EFor state (node: for_loop reg) =
  let Region.{region; value} = node in
  let {index; bound1; direction; bound2; body; _} = value in

  let print_direction state = function
    To     kwd -> Tree.make_node ~region:kwd#region state "To"
  | Downto kwd -> Tree.make_node ~region:kwd#region state "DownTo" in

  let children =
    mk_children_var_pattern index.value
    @ Tree.[
        mk_child print_expr      bound1;
        mk_child print_direction direction;
        mk_child print_expr      bound2;
        mk_child print_loop_body body]
  in Tree.make state ~region "EFor" children

and print_loop_body state (node: loop_body) =
  match node.seq_expr with
    None      -> ()
  | Some body -> Tree.of_nsepseq state "<body>" print_expr body

(* For-in loop *)

and print_EForIn state (node: for_in_loop reg) =
  let Region.{region; value} = node in
  let {pattern; collection; body; _} = value in
  let children = Tree.[
    mk_child print_pattern   pattern;
    mk_child print_expr      collection;
    mk_child print_loop_body body]
  in Tree.make state ~region "EForIn" children

(* While-loop *)

and print_EWhile state (node: while_loop reg) =
  let Region.{region; value} = node in
  let children = Tree.[
    mk_child print_expr      value.cond;
    mk_child print_loop_body value.body]
  in Tree.make state ~region "EWhile" children

(* TYPE EXPRESSIONS *)

and print_type_expr state = function
  TProd   t -> print_TProd   state t
| TSum    t -> print_TSum    state t
| TRecord t -> print_TRecord state t
| TApp    t -> print_TApp    state t
| TFun    t -> print_TFun    state t
| TPar    t -> print_TPar    state t
| TVar    t -> print_TVar    state t
| TString t -> print_TString state t
| TInt    t -> print_TInt    state t
| TModA   t -> print_TModA   state t
| TArg    t -> print_TArg    state t

(* Product type *)

and print_TProd state (node: cartesian) =
  let Region.{region; value} = node in
  Tree.of_nsepseq state ~region "TProd"print_type_expr value

(* Variant type *)

and print_TSum state (node: sum_type reg) =
  let Region.{region; value} = node in
  let children =
    Tree.mk_children_nsepseq print_variant value.variants
  @ mk_children_attr value.attributes
  in Tree.make state ~region "TSum" children

and print_variant state (node : variant reg) =
  let node = node.value in
  let children = Tree.mk_child_opt print_of_type_expr node.arg
                 :: mk_children_attr node.attributes in
  let region = node.constr#region
  and root   = node.constr#payload in
  Tree.make state root ~region children

and print_of_type_expr state (_, type_expr) =
  print_type_expr state type_expr

(* Record type *)

and print_TRecord state (node: field_decl reg ne_injection reg) =
  let print_field_decl state (node: field_decl reg) =
    let {field_name; field_type; attributes; _} = node.value in
    let children = Tree.[
      mk_child make_literal    field_name;
      mk_child print_type_expr field_type]
    @ mk_children_attr attributes
    in Tree.make state "<field>" children in

  let Region.{value; region} = node in

  let children =
    Tree.mk_children_nsepseq print_field_decl value.ne_elements
    @ mk_children_attr value.attributes
  in Tree.make state ~region "RRecord" children

(* Application of type constructors *)

and print_TApp state (node: (type_constr * type_constr_arg) reg) =
  let Region.{value; region} = node in

  let print_type_constr_arg state = function
    CArg e ->
      Tree.make_unary state "CArg" print_type_expr e
  | CArgTuple e ->
      let seq = e.value.inside in
      Tree.of_nsepseq state "CArgTuple" print_type_expr seq in

  let name, tuple = value in
  let children = Tree.[
    mk_child make_literal          name;
    mk_child print_type_constr_arg tuple]
  in Tree.make state ~region "TApp" children

(* Functional types *)

and print_TFun state (node: (type_expr * arrow * type_expr) reg) =
  let Region.{value; region} = node in
  let domain, _, codomain = value in
  let children = Tree.[
    mk_child print_type_expr domain;
    mk_child print_type_expr codomain]
  in Tree.make state ~region "TFun" children

(* Parenthesises type *)

and print_TPar state (node: type_expr par reg) =
  let Region.{region; value} = node in
  Tree.make_unary state ~region "TPar" print_type_expr value.inside

(* Type variable *)

and print_TVar state (node: variable) =
  Tree.(make_unary state "TVar" make_literal node)

(* Type string *)

and print_TString state (node: lexeme wrap) =
  Tree.(make_unary state "TString" make_string node)

(* Integer type *)

and print_TInt state (node: (lexeme * Z.t) wrap) =
  Tree.make_int "TInt" state node

(* Type from module path *)

and print_TModA state (node: type_expr module_access reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child make_literal    value.module_name;
    mk_child print_type_expr value.field]
  in Tree.make state ~region "TModA" children

(* Type argument *)

and print_TArg state (node: type_var reg) =
  Tree.make_unary state "TArg" print_type_var node

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
