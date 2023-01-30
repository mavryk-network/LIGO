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
type ('a, 'sep)  sepseq = ('a, 'sep) Utils.sepseq

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
  let _, kwd_rec, let_binding, attr = value in
  let {type_params; binders; rhs_type; let_rhs; _} = let_binding in

  let print_type_params state (node: type_params par reg) =
    let Region.{region; value} = node in
    Tree.of_nseq state ~region "<type parameters>" value.inside in

  let children = Tree.[
    mk_child_opt make_literal          kwd_rec;
    mk_child_opt print_type_params     type_params;
    mk_child     print_binders         binders;
    mk_child_opt print_type_annotation rhs_type;
    mk_child     print_expr            let_rhs]
  in Tree.make ~region state "Let" children

and print_binders state (node: pattern nseq) =
  Tree.of_nseq state "<binders>" print_pattern node in

and print_type_annotation state (_, type_expr) =
  Tree.make_unary state "<type>" print_type_expr type_expr

(* Type declaration *)

and print_TypeDecl state (node: type_decl reg) =
  let Region.{value; region} = node in
  let {name; params; type_expr; _} = value in

  let print_QParam state (node: type_var reg) =
    let Region.{value; region} = node in
    Tree.make_unary state ~region "QParam" print_type_var value

  and print_QParamTuple state (node: (type_var reg, comma) nsepseq par reg) =
    let Region.{value; region} = node in
    Tree.of_nsepseq state ~region state "QParamTuple" value.inside

  let print_type_vars state = function
    QParam      tv -> print_QParam state tv
  | QParamTuple tv -> print_QParamTuple state tv in

  let children = Tree.[
    mk_child     make_literal    name;
    mk_child_opt print_type_vars params;
    mk_child     print_type_expr type_expr]
  in Tree.make ~region state "TypeDecl" children

and print_type_var state (node: type_var) =
  Tree.make_literal state ("'" ^ node.name)

(* Module declaration *)

and print_ModuleDecl state (node: module_decl reg) =
  let Region.{value; region} = node in
  let children = Tree.[
    mk_child make_literal value.name;
    mk_child print_cst    value.module_]
  in Tree.make state ~region "ModuleDecl" children

(* Module aliases *)

and print_ModuleAlias state (node: module_alias reg) =
  let Region.{value; region} = node in

  let print_path state (node: (module_name, dot) nsepseq) =
    Tree.of_nsepseq state "<path>" make_literal node in

  let children = Tree.[
    mk_child make_literal value.alias;
    mk_child print_path   value.binders]
  in Tree.make state ~region "ModuleAlias" children

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
  let children = Tree.(mk_child make_literal value.variable)
                 :: mk_children_attr value.attributes
  in Tree.make state ~region "PVar" children

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

(* Unit pattern *)

and print_PUnit state (node: the_unit reg) =
  let Region.{region; value} = node in
  Tree.make_node state ~region "()"

(* List patterns *)

and print_PList state = function
  PListComp p -> print_PListComp state p
| PCons     p -> print_PConst    state p

and print_ListComp state (node: pattern injection reg) =
  print_injection print_pattern state node.value

and print_PCons state (node: (pattern * cons * pattern) reg) =
  let Region.{region; value} = node in
  let hd_pattern, _, tl_pattern = value in
  let children = Tree.[
    mk_child print_pattern hd_pattern;
    mk_child print_pattern tl_pattern]
  in Tree.make state ~region "PCons" children

and print_injection :
  type a.a Tree.printer -> Tree.state -> a injection -> unit =
  fun print state inj -> Tree.of_sepseq state print inj

(* Tuple patterns *)

and print_PTuple state (node: (pattern, comma) nsepseq reg) =
  print_loc_node state "PTuple" t.region;
  print_tuple_pattern (state#pad 1 0) t.value

and print_PPar state (node: pattern par reg) =
{value; _} ->
    print_node state "PPar";
    print_pattern (state#pad 1 0) value.inside

and print_PRecord state (node: field_pattern reg ne_injection reg) =
  {value; _} ->
    print_node state "PRecord";
    print_ne_injection print_field_pattern state value

and print_PTyped state (node: typed_pattern reg) =
{value; _} ->
    print_node state "PTyped";
    print_typed_pattern state value

and print_field_pattern state {value; _} =
  print_node    state value.field_name.value;
  print_pattern (state#pad 1 0) value.pattern

and print_typed_pattern state node =
  print_pattern   (state#pad 2 0) node.pattern;
  print_type_expr (state#pad 2 1) node.type_expr

and print_tuple_pattern state tuple =
  let patterns       = Utils.nsepseq_to_list tuple in
  let length         = List.length patterns in
  let apply len rank = print_pattern (state#pad len rank)
  in List.iteri ~f:(apply length) patterns


and print_ne_injection :
  'a.(state -> 'a -> unit) -> state -> 'a ne_injection -> unit =
  fun printer state inj ->
    let ne_elements = Utils.nsepseq_to_list inj.ne_elements in
    let length      = List.length ne_elements in
    let arity       = if List.is_empty inj.attributes then length else length+1
    and apply len rank = printer (state#pad len rank)
    in List.iteri ~f:(apply arity) ne_elements;
       if not (List.is_empty inj.attributes) then
         let state = state#pad arity (arity-1)
         in print_attributes state inj.attributes

and print_record_type state = print_ne_injection print_field_decl state

and print_bytes state {value=lexeme,hex; region} =
  print_loc_node (state#pad 2 0) lexeme region;
  print_node     (state#pad 2 1) (Hex.show hex)

and print_int state {value=lexeme,z; region} =
  print_loc_node (state#pad 2 0) lexeme region;
  print_node     (state#pad 2 1) (Z.to_string z)

and print_mutez state {value=lexeme,int64; region} =
  print_loc_node (state#pad 2 0) lexeme region;
  print_node     (state#pad 2 1) (Int64.to_string int64)

and print_constr_pattern state {value; _} =
  let constr, pat_opt = value in
  print_ident state constr;
  match pat_opt with
    None -> ()
  | Some pat -> print_pattern state pat

and print_loop_body state { kwd_do = _; seq_expr; kwd_done = _ } =
  match seq_expr with
  | None -> print_node (state#pad 1 0) "<empty>"
  | Some exprs ->
      let exprs = Utils.nsepseq_to_list exprs in
      let n_exprs = List.length exprs in
      List.iteri exprs ~f:(fun i expr -> print_expr (state#pad n_exprs i) expr)

and print_direction state direction =
  match direction with
  | To kwd | Downto kwd -> print_loc_node state kwd#payload kwd#region

and print_index state index = print_PVar state index

and print_for_loop state
    { kwd_for = _; index; equal = _; bound1; direction; bound2; body } =
  let () =
    let state = state#pad 5 0 in
    print_node state "<index>";
    print_index (state#pad 1 0) index
  in
  let () =
    let state = state#pad 5 1 in
    print_node state "<bound1>";
    print_expr (state#pad 1 0) bound1
  in
  let () =
    let state = state#pad 5 2 in
    print_node state "<direction>";
    print_direction (state#pad 1 0) direction
  in
  let () =
    let state = state#pad 5 3 in
    print_node state "<bound2>";
    print_expr (state#pad 1 0) bound2
  in
  let () =
    let state = state#pad 5 4 in
    print_node state "<body>";
    print_loop_body (state#pad 1 0) body
  in
  ()

and print_for_in_loop state
    { kwd_for = _; pattern; kwd_in = _; collection; body } =
  let () =
    let state = state#pad 3 0 in
    print_node state "<pattern>";
    print_pattern (state#pad 1 0) pattern
  in
  let () =
    let state = state#pad 3 1 in
    print_node state "<collection>";
    print_expr (state#pad 1 0) collection
  in
  let () =
    let state = state#pad 3 2 in
    print_node state "<body>";
    print_loop_body (state#pad 1 0) body
  in
  ()

and print_while_loop state
    { kwd_while = _; cond; body } =
  let () =
    let state = state#pad 2 0 in
    print_node state "<cond>";
    print_expr (state#pad 1 0) cond
  in
  let () =
    let state = state#pad 2 1 in
    print_node state "<body>";
    print_loop_body (state#pad 1 0) body
  in
  ()

and print_expr state = function
  ECase {value; region} ->
    print_loc_node state "ECase" region;
    print_case print_expr state value
| ECond {value; region} ->
    print_loc_node state "ECond" region;
    print_cond_expr state value
| EAnnot {value; region} ->
    print_loc_node  state "EAnnot" region;
    print_annotated state value
| ELogic e_logic ->
    print_node state "ELogic";
    print_e_logic (state#pad 1 0) e_logic
| EArith e_arith ->
    print_node state "EArith";
    print_arith_expr (state#pad 1 0) e_arith
| EString e_string ->
    print_node state "EString";
    print_string_expr (state#pad 1 0) e_string
| EList e_list ->
    print_node state "EList";
    print_list_expr (state#pad 1 0) e_list
| EConstr e_constr ->
    print_node state "EConstr";
    print_constr_expr (state#pad 1 0) e_constr
| ERecord {value; region} ->
    print_loc_node state "ERecord" region;
    print_ne_injection print_field_assign state value
| EProj {value; region} ->
    print_loc_node state "EProj" region;
    print_projection state value
| EModA {value; region} ->
    print_loc_node state "EModA" region;
    print_module_access print_expr state value
| EUpdate {value; region} ->
    print_loc_node state "EUpdate" region;
    print_update state value
| EVar v ->
    print_node  state "EVar";
    print_ident (state#pad 1 0) v
| ECall {value; region} ->
    print_loc_node state "ECall" region;
    print_fun_call state value
| EBytes b ->
    print_node state "EBytes";
    print_bytes state b
| EUnit u ->
    print_loc_node state "EUnit" u.region
| ETuple e_tuple ->
    print_node state "ETuple";
    print_tuple_expr state e_tuple
| EPar {value; region} ->
    print_loc_node state "EPar" region;
    print_expr (state#pad 1 0) value.inside
| ELetIn {value; region} ->
    print_loc_node state  "ELetIn" region;
    print_let_in state value
| ELetMutIn {value; region} ->
    print_loc_node state "ELetMutIn" region;
    print_let_mut_in state value
| ETypeIn {value; region} ->
    print_loc_node state  "ETypeIn" region;
    print_type_in state value
| EModIn {value; region} ->
    print_loc_node state  "EModIn" region;
    print_mod_in state value
| EModAlias {value; region} ->
    print_loc_node state  "EModAlias" region;
    print_mod_alias state value
| EFun {value; region} ->
    print_loc_node state "EFun" region;
    print_fun_expr state value
| ESeq {value; region} ->
    print_loc_node state "ESeq" region;
    print_injection print_expr state value
| ECodeInj {value; region} ->
    print_loc_node state "ECodeInj" region;
    print_code_inj state value
| ERevApp {value; region} ->
    print_bin_op "ERevApp" region state value
| EAssign {value; region} ->
    print_loc_node state "EAssign" region;
    print_assign state value
| EFor {value; region} ->
    print_loc_node state "EFor" region;
    print_for_loop state value
| EForIn {value; region} ->
    print_loc_node state "EForIn" region;
    print_for_in_loop state value
| EWhile {value; region} ->
    print_loc_node state "EWhile" region;
    print_while_loop state value

and print_assign state (assign : CST.assign) =
  let { binder; ass = _; expr } = assign in
  let () =
    let state = state#pad 2 0 in
    print_node state "<binder>";
    print_ident (state#pad 1 0) binder
  in
  let () =
    let state = state#pad 2 1 in
    print_node state "<expr>";
    print_expr (state#pad 1 0) expr
  in
  ()

and print_module_access :
  type a. (state -> a -> unit ) -> state -> a module_access -> unit =
  fun f state ma ->
    print_ident (state#pad 2 0) ma.module_name;
    f (state#pad 2 1) ma.field

and print_fun_expr state node =
  let {binders; rhs_type; body; _} = node in
  let arity = if Option.is_none rhs_type then 2 else 3 in
  let () =
    let state = state#pad arity 0 in
    print_node state "<parameters>";
    print_binders state binders in
  let () =
    match rhs_type with
      None -> ()
    | Some (_, type_expr) ->
       let state = state#pad arity 1 in
       print_node state "<lhs type>";
       print_type_expr (state#pad 1 0) type_expr in
  let () =
    let state = state#pad arity (arity - 1) in
    print_node state "<body>";
    print_expr (state#pad 1 0) body
  in ()

and print_code_inj state rc =
  let () =
    let state = state#pad 2 0 in
    print_node state "<language>";
    print_string (state#pad 1 0) rc.language.value in
  let () =
    let state = state#pad 2 1 in
    print_node state "<code>";
    print_expr (state#pad 1 0) rc.code
  in ()

and print_let_in state node =
  let {binding; body; attributes; kwd_rec; _} = node in
  let {binders; rhs_type; let_rhs; _} = binding in
  let arity = if Option.is_none rhs_type then 3 else 4 in
  let arity = if Option.is_none kwd_rec then arity else arity+1 in
  let arity = if List.is_empty attributes then arity else arity+1 in
  let rank =
    match kwd_rec with
      None -> 0
    | Some (_) ->
      let state = state#pad arity 0 in
      print_node state "rec"; 0 in
  let rank =
    let state = state#pad arity 0 in
    print_node state "<binders>";
    print_binders state binders; rank in
  let rank =
    match rhs_type with
      None -> rank
    | Some (_, type_expr) ->
       let state = state#pad arity (rank+1) in
       print_node state "<lhs type>";
       print_type_expr (state#pad 1 0) type_expr;
       rank+1 in
  let rank =
    let state = state#pad arity (rank+1) in
    print_node state "<rhs>";
    print_expr (state#pad 1 0) let_rhs;
    rank+1 in
  let rank =
    let state = state#pad arity (rank+1) in
    print_node state "<body>";
    print_expr (state#pad 1 0) body;
    rank+1 in
  let () =
    if not (List.is_empty attributes) then
      let state = state#pad arity (rank+1)
      in print_attributes state attributes
  in ()

and print_let_mut_in state node =
  let {binding; body; attributes; kwd_mut = _; _} = node in
  let {binders; rhs_type; let_rhs; _} = binding in
  let arity = if Option.is_none rhs_type then 3 else 4 in
  let arity = if List.is_empty attributes then arity else arity+1 in
  let rank =
    let state = state#pad arity 0 in
    print_node state "<binders>";
    print_binders state binders; 0 in
  let rank =
    match rhs_type with
      None -> rank
    | Some (_, type_expr) ->
       let state = state#pad arity (rank+1) in
       print_node state "<lhs type>";
       print_type_expr (state#pad 1 0) type_expr;
       rank+1 in
  let rank =
    let state = state#pad arity (rank+1) in
    print_node state "<rhs>";
    print_expr (state#pad 1 0) let_rhs;
    rank+1 in
  let rank =
    let state = state#pad arity (rank+1) in
    print_node state "<body>";
    print_expr (state#pad 1 0) body;
    rank+1 in
  let () =
    if not (List.is_empty attributes) then
      let state = state#pad arity (rank+1)
      in print_attributes state attributes
  in ()

and print_type_in state node =
  let {type_decl; body; _} = node in
  let {name; type_expr; _} = type_decl in
  let () =
    let state = state#pad 3 0 in
    print_node  state "<name>";
    print_ident state name in
  let () =
    let state = state#pad 3 1 in
    print_node state "<type>";
    print_type_expr (state#pad 1 0) type_expr in
  let () =
    let state = state#pad 3 2 in
    print_node state "<body>";
    print_expr (state#pad 1 0) body
  in ()

and print_mod_in state node =
  let {mod_decl; body; _} = node in
  let {name; module_; _} = mod_decl in
  let () =
    let state = state#pad 3 0 in
    print_node state "<name>";
    print_ident state name in
  let () =
    let state = state#pad 3 1 in
    print_node state "<module>";
    print_cst (state#pad 1 0) module_ in
  let () =
    let state = state#pad 3 2 in
    print_node state "<body>";
    print_expr (state#pad 1 0) body
  in ()

and print_mod_alias state node =
  let {mod_alias; body; _} = node in
  let {alias;binders; _} = mod_alias in
  let () =
    let state = state#pad 3 0 in
    print_node  state "<alias>";
    print_ident state alias in
  let () =
    let state          = state#pad 3 1 in
    let binders        = Utils.nsepseq_to_list binders in
    let len            = List.length binders in
    let apply len rank = print_ident (state#pad len rank) in
    print_node state "<module>";
    List.iteri ~f:(apply len) binders in
  let () =
    let state = state#pad 3 2 in
    print_node state "<body>";
    print_expr (state#pad 1 0) body
  in ()

(*
and print_attributes state attributes =
  let apply {value = attribute; region} =
    let attribute_formatted = sprintf "[@%s]" attribute in
    let token = Token.wrap attribute_formatted region in
    print_token state token attribute_formatted
  in List.iter ~f:apply attributes
 *)

and print_tuple_expr state {value; _} =
  let exprs          = Utils.nsepseq_to_list value in
  let length         = List.length exprs in
  let apply len rank = print_expr (state#pad len rank)
  in List.iteri ~f:(apply length) exprs

and print_fun_call state (fun_expr, args) =
  let args           = Utils.nseq_to_list args in
  let arity          = List.length args in
  let apply len rank = print_expr (state#pad len rank)
  in print_expr (state#pad (1+arity) 0) fun_expr;
     List.iteri ~f:(apply arity) args

and print_projection state proj =
  let selections     = Utils.nsepseq_to_list proj.field_path in
  let len            = List.length selections in
  let apply len rank = print_selection (state#pad len rank) in
  print_ident (state#pad (1+len) 0) proj.struct_name;
  List.iteri ~f:(apply len) selections

and print_update state update =
  print_path (state#pad 2 0) update.record;
  print_ne_injection print_field_path_assign state update.updates.value

and print_path state = function
  Name name ->
    print_node state "Name";
    print_ident (state#pad 1 0) name
| Path {value; region} ->
    print_loc_node state "Path" region;
    print_projection state value

and print_selection state = function
  FieldName fn ->
    print_node state "FieldName";
    print_ident (state#pad 1 0) fn
| Component c ->
    print_node state "Component";
    print_int state c

and print_field_assign state {value; _} =
  match value with
    Property {field_name; field_expr; _} ->
      print_node  state  "<field assignment>";
      print_ident (state#pad 2 0) field_name;
      print_expr  (state#pad 2 1) field_expr
  | Punned_property field_name ->
    print_node  state  "<punned field assignment>";
    print_ident (state#pad 2 0) field_name

and print_field_path_assign state {value; _} =
  match value with
    Path_property {field_path; field_expr; _} ->
      print_node state "<update>";
      print_path (state#pad 2 0) field_path;
      print_expr (state#pad 2 1) field_expr
  | Path_punned_property field_name ->
    print_node state "<punned update>";
    print_ident (state#pad 2 0) field_name

and print_constr_expr state {value; _} =
  let constr, expr_opt = value in
  match expr_opt with
    None -> print_ident (state#pad 1 0) constr
  | Some expr ->
     print_ident (state#pad 2 0) constr;
     print_expr  (state#pad 2 1) expr

and print_list_expr state = function
  ECons {value; region} ->
    print_loc_node state "ECons" region;
    print_expr (state#pad 2 0) value.arg1;
    print_expr (state#pad 2 1) value.arg2
| EListComp {value; region} ->
    print_loc_node state "EListComp" region;
    if   Option.is_none value.elements
    then print_node (state#pad 1 0) "<nil>"
    else print_injection print_expr state value

and print_string_expr state = function
  Cat {value; region} ->
    print_loc_node state "Cat" region;
    print_expr (state#pad 2 0) value.arg1;
    print_expr (state#pad 2 1) value.arg2;
| String s ->
    print_node   state "String";
    print_string (state#pad 1 0) s
| Verbatim v ->
    print_node   state "Verbatim";
    print_string (state#pad 1 0) v

and print_arith_expr state = function
  Add {value; region} ->
    print_bin_op "Add" region state value
| Sub {value; region} ->
    print_bin_op "Sub" region state value
| Mult {value; region} ->
    print_bin_op "Mult" region state value
| Div {value; region} ->
    print_bin_op "Div" region state value
| Mod {value; region} ->
    print_bin_op "Mod" region state value
| Land {value; region} ->
    print_bin_op "Land" region state value
| Lor {value; region} ->
    print_bin_op "Lor" region state value
| Lxor {value; region} ->
    print_bin_op "Lxor" region state value
| Lsl {value; region} ->
    print_bin_op "Lsl" region state value
| Lsr {value; region} ->
    print_bin_op "Lsr" region state value
| Neg {value; region} ->
    print_loc_node state "Neg" region;
    print_expr (state#pad 1 0) value.arg;
| Int i ->
    print_node state "Int";
    print_int  state i
| Nat n ->
    print_node state "Nat";
    print_int  state n
| Mutez m ->
    print_node state "Mutez";
    print_mutez  state m

and print_e_logic state = function
  BoolExpr e ->
    print_node state "BoolExpr";
    print_bool_expr (state#pad 1 0) e
| CompExpr e ->
    print_node state "CompExpr";
    print_comp_expr (state#pad 1 0) e

and print_bool_expr state = function
  Or {value; region} ->
    print_bin_op "Or" region state value
| And {value; region} ->
    print_bin_op "And" region state value
| Not {value; _} ->
    print_node state "Not";
    print_expr (state#pad 1 0) value.arg

and print_comp_expr state = function
  Lt {value; region} ->
    print_bin_op "Lt" region state value
| Leq {value; region} ->
    print_bin_op "Leq" region state value
| Gt {value; region} ->
    print_bin_op "Gt" region state value
| Geq {value; region} ->
    print_bin_op "Geq" region state value
| Equal {value; region} ->
    print_bin_op "Equal" region state value
| Neq {value; region} ->
    print_bin_op "Neq" region state value

and print_bin_op node region state op =
  print_loc_node state node region;
  print_expr (state#pad 2 0) op.arg1;
  print_expr (state#pad 2 1) op.arg2

and print_annotated state annot =
  let expr, _, t_expr = annot.inside in
  print_expr      (state#pad 2 0) expr;
  print_type_expr (state#pad 2 1) t_expr

and print_cond_expr state (cond: cond_expr) =
  let arity = if Option.is_none cond.ifnot then 2 else 3 in
  let () =
    let state = state#pad arity 0 in
    print_node state "<condition>";
    print_expr (state#pad 1 0) cond.test in
  let () =
    let state = state#pad arity 1 in
    print_node state "<true>";
    print_expr (state#pad 1 0) cond.ifso in
  let () = match cond.ifnot with
    Some (_, ifnot) ->
      let state = state#pad arity 2 in
      print_node state "<false>";
      print_expr (state#pad 1 0) ifnot
  | None -> ()
  in ()

and print_case :
  'a.(state -> 'a -> unit) -> state -> 'a case -> unit =
  fun printer state case ->
  let clauses = Utils.nsepseq_to_list case.cases.value in
  let clauses = List.map ~f:(fun x -> x.value) clauses in
  let arity  = List.length clauses + 1 in
  let apply len rank =
    print_case_clause printer (state#pad len (rank+1))
  in print_expr (state#pad arity 0) case.expr;
     List.iteri ~f:(apply arity) clauses

and print_case_clause :
  'a.(state -> 'a -> unit) -> state -> 'a case_clause -> unit =
  fun printer state clause ->
  print_node    state "<clause>";
  print_pattern (state#pad 2 0) clause.pattern;
  printer    (state#pad 2 1) clause.rhs

and print_type_expr state = function
  TProd {value; region} ->
    print_loc_node state "TProd" region;
    print_cartesian state value
| TSum {value; region} ->
    print_loc_node state "TSum" region;
    print_sum_type state value
| TRecord {value; region} ->
    print_loc_node    state "TRecord" region;
    print_record_type state value
| TApp {value; region} ->
    let name, tuple = value in
    print_loc_node        state "TApp" region;
    print_ident           (state#pad 2 0) name;
    print_type_constr_arg (state#pad 2 1) tuple
| TFun {value; region} ->
    print_loc_node state "TFun" region;
    let apply len rank =
      print_type_expr (state#pad len rank) in
    let domain, _, range = value in
    List.iteri ~f:(apply 2) [domain; range]
| TPar {value={inside;_}; region} ->
    print_loc_node  state "TPar" region;
    print_type_expr (state#pad 1 0) inside
| TVar v ->
    print_node  state "TVar";
    print_ident (state#pad 1 0) v
| TString s ->
    print_node   state "TString";
    print_string (state#pad 1 0) s
| TInt s ->
    print_node   state "TInt";
    print_int (state#pad 1 0) s
| TModA {region; value} ->
    print_loc_node state "TModA" region;
    print_module_access print_type_expr state value
| TArg t ->
    print_node state "TArg";
    print_type_var (state#pad 1 0) t

and print_sum_type state {variants; attributes; _} =
  let variants = Utils.nsepseq_to_list variants in
  let arity    = List.length variants in
  let arity    = if List.is_empty attributes then arity else arity+1 in
  let apply arity rank variant =
    let state = state#pad arity rank in
    print_variant state variant.value in
  let () = List.iteri ~f:(apply arity) variants in
  if not (List.is_empty attributes) then
    let state = state#pad arity (arity-1)
    in print_attributes state attributes

and print_type_constr_arg state = function
  CArg  t -> print_type_expr state t
| CArgTuple t -> print_arg_tuple state t

and print_arg_tuple state node =
  let {value={inside; _}; _} = node in
  let args = Utils.nsepseq_to_list inside in
  let arity = List.length args in
  let apply len rank = print_type_expr (state#pad len rank)
  in List.iteri ~f:(apply arity) args

and print_field_decl state {value; _} =
  let arity = if List.is_empty value.attributes then 1 else 2 in
  print_ident     state value.field_name;
  print_type_expr (state#pad arity 0) value.field_type;
  if not (List.is_empty value.attributes) then
    print_attributes (state#pad arity 1) value.attributes

and print_cartesian state t_exprs =
  let t_exprs        = Utils.nsepseq_to_list t_exprs in
  let arity          = List.length t_exprs in
  let apply len rank = print_type_expr (state#pad len rank)
  in List.iteri ~f:(apply arity) t_exprs

and print_variant state {constr; arg; attributes=attr} =
  let arity = if List.is_empty attr then 0 else 1 in
  let arity = if Option.is_none arg then arity else arity + 1 in
  let rank  = 0 in
  let () = print_ident state constr in
  let rank =
    match arg with
      None -> rank
    | Some (_,c) ->
        print_type_expr (state#pad arity rank) c; rank+1 in
  let () = if not (List.is_empty attr) then
             print_attributes (state#pad arity rank) attr
  in ()

(* PRINTING (client-slide) *)

type ('src, 'dst) printer = Tree.state -> 'src -> 'dst

let print_to_buffer state cst = print_cst state cst; state#buffer

let print_to_string state cst =
  Buffer.contents (print_to_buffer state cst)

let print_pattern_to_string state pattern =
  print_pattern state pattern; Buffer.contents state#buffer

(* Aliases *)

let to_buffer = print_to_buffer
let to_string = print_to_string
let pattern_to_string = print_pattern_to_string
