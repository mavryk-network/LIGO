[@@@warning "-30"]
(*
  Warning 30 is triggered on multiply-defined record labels
  For example :
    type record_a = { body : type_a ; foo : int }
    type record_b = { body : type_b ; bar : string } 
  Here, both record_a and record_b have a field called [body].
  It triggers warning 30.

  CAREFUL : If record labels are defined multiple times,
  they should always the same type.
  Otherwise, this causes several errors with the json ppx.
  (in above example, record_a's body and record_b's body should have the same type)
*)

open Ligo_prim

module Non_linear_rows = Temp_prim.Non_linear_rows(Label)
module Empty_label = struct
  type t = unit [@@deriving eq, compare, yojson, hash]
  let t_of_sexp _ = ()
  let sexp_of_t _ = failwith "wait"
  let of_string _ = ()
  let to_string () = "unlabeled"
end
module Non_linear_disc_rows = Temp_prim.Non_linear_rows(Empty_label)
module Attribute = Temp_prim.Attribute

module Location = Simple_utils.Location
module List = Simple_utils.List
module Label = Ligo_prim.Label

module Z = Literal_value.Z

type 'a nseq = 'a Simple_utils.List.Ne.t
  [@@deriving yojson]
type attribute = Temp_prim.Attribute.t

(* The preprocessor directives are left unchanged during unification pass.
   So, the type of a directive is Preprocessor.Directive.t
   in both the CST and here in the AST unified.
   However, the Directive module is augmented here with
   yojson functions for the yojson ppx to work correctly. *)
module Directive = struct
  include Preprocessor.Directive

  let to_yojson : t -> Yojson.Safe.t =
    fun _ -> `String "JSON printing of directives is not supported"
  let dummy_directive = PP_Endif Region.ghost
  let of_yojson : Yojson.Safe.t -> (t, string) Result.t =
    fun _ -> Error "JSON parsing of directive is not supported"
end


type ('lhs, 'rhs) field =
| Punned of 'lhs
| Complete of ('lhs * 'rhs)
  [@@deriving yojson]

(* ========================== TYPES ======================================== *)

type type_expression = {
  type_expression_content : type_expression_content;
  location                : Location.t
}
and type_expr_content = type_expression_content
and type_expr         = type_expression
  [@@deriving yojson]

and 'constr type_app = {
  constr     : 'constr;
  type_args  : type_expr nseq;
}

and 'a module_access = {
  module_name : string;
  field       : 'a;
}

and 'a module_path = {
  module_path : string nseq;
  field       : 'a;
}

and fun_type_arg = {
  name      : string;
  type_expr : type_expr
}
and fun_type_args = fun_type_arg nseq

and type_expression_content =
| T_Prod         of type_expr nseq
| T_App          of string type_app
| T_Fun          of type_expr * type_expr
| T_Named_fun     of fun_type_args * type_expr
| T_Par          of type_expr
| T_Var          of string
| T_String       of string
| T_Int          of string * Z.t
| T_ModA         of type_expr module_access
| T_Arg          of string
| T_Sum_raw      of type_expr option Non_linear_rows.t
(* "curried" sum-type ["Ctor", arg1, arg2, arg3 ]*)
| T_Arg_sum_raw  of type_expr list option Non_linear_rows.t
| T_Record_raw   of type_expr option Non_linear_rows.t
| T_Disc_union   of type_expr Non_linear_disc_rows.t
| T_Attr         of Attribute.t * type_expr
| T_AppPascaligo of type_expr type_app
| T_ModPath      of type_expr module_path

(* ========================== PATTERNS ===================================== *)

and pattern = {
  pattern_content : pattern_content;
  location        : Location.t
}
and ptrn_content = pattern_content
and ptrn         = pattern
  [@@deriving yojson]

and list_pattern =
| PListComp of pattern list
| PCons     of pattern * pattern

and assign_pattern = {
  property  : string;
  value     : expr
}

and let_binding = {
  is_rec      : bool;
  type_params : string nseq option;
  binders     : pattern nseq;
  rhs_type    : type_expr option;
  let_rhs     : expression;
}

and destruct = {
  property  : string;
  target    : let_binding;
}

and 'a field_assign = {
  name : string;
  expr : 'a; 
} [@@deriving yojson]


and pattern_content =
(* Shared *)
| P_Constr   of string * ptrn option
| P_Unit
| P_Var      of string
| P_Int      of string * Z.t
| P_Nat      of string * Z.t
| P_Bytes    of string * bytes (* No [Hex.to_yojson], hence [bytes] instead *)
| P_String   of string
| P_Verbatim of string
| P_List     of list_pattern
| P_Tuple    of ptrn nseq
| P_Par      of ptrn
| P_Typed    of ptrn * type_expr option
(* Cameligo *)
| P_RecordCameligo   of ptrn field_assign nseq
(* Pascaligo *)
| P_App      of ptrn * ptrn nseq option
| P_Attr     of Attribute.t * ptrn
| P_ModPath  of ptrn module_path
| P_Mutez    of string * Int64.t
| P_Nil
| P_Ctor     of string
| P_RecordPascaligo   of (ptrn, ptrn) field list
(* Jsligo *)
| P_Rest     of string
| P_Assign   of assign_pattern
| P_Destruct of destruct
| P_Object   of ptrn nseq
| P_Array    of ptrn nseq

(* P_App (P_Ctor x, ...) |-> P_Constr (x, ...)
P_RecordCameligo |-> P_Record  // ne-list into list *)

(* ========================== INSTRUCTIONS ================================= *)
and instruction = {
  instruction_content : instruction_content;
  location                : Location.t
}
and instr_content = instruction_content
and instr         = instruction
  [@@deriving yojson]

and block_pascaligo = statement_pascaligo nseq

and 'clause case_clause = {
  pattern : pattern;
  rhs     : 'clause;
}

and test_clause =
| ClauseInstr of instruction
| ClauseBlock of block_pascaligo

and 'clause case = {
  expr         : expr;
  cases        : 'clause case_clause nseq;
}

and assignment = {
  lhs_expr : expr;
  rhs_expr : expr;
}

and 'branch cond = {
  test         : expr;
  ifso         : 'branch;
  ifnot        : 'branch option;
}

and for_int = {
  index   : string;
  init    : expr;
  bound   : expr;
  step    : expr option; (* [1] if [None] *)
  block   : block_pascaligo;
}

and for_in =
| ForMap       of for_map
| ForSetOrList of for_set_or_list

and for_map = {
  binding    : string * string;
  collection : expr;
  block      : block_pascaligo;
}

and for_set_or_list = {
  var        : string;
  for_kind   : [`Set | `List];
  collection : expr;
  block      : block_pascaligo;
}

and patch = {
  collection : expr;
  patch_kind : [`Map | `Record | `Set];
  patch      : expr;
}

and removal = {
  item_expr   : expr;
  remove_kind : [`Set | `Map];
  collection  : expr;
}

and while_loop = {
  cond      : expr;
  block     : block_pascaligo;
}

and instruction_content =
| I_Assign of assignment
| I_Call   of expr * expr list
| I_Case   of test_clause case
| I_Cond   of test_clause cond
| I_For    of for_int
| I_ForIn  of for_in
| I_Patch  of patch
| I_Remove of removal
| I_Skip
| I_While  of while_loop

(* ========================== STATEMENTS PASCALIGO ========================= *)

and statement_pascaligo = {
  statement_pascaligo_content : statement_pascaligo_content;
  location                    : Location.t
}
and stmt_pascaligo_content = statement_pascaligo_content
and stmt_pascaligo         = statement_pascaligo
  [@@deriving yojson]


and var_decl = {
  pattern     : pattern;
  type_params : Type_var.t nseq option;
  var_type    : type_expression option;
  init        : expr;
}

and statement_pascaligo_content =
| S_Attr      of (Attribute.t * statement_pascaligo) 
| S_Decl      of declaration
| S_Instr     of instruction
| S_VarDecl   of var_decl

(* ========================== STATEMENTS JSLIGO ============================ *)

and statement_jsligo = {
  statement_jsligo_content : statement_jsligo_content;
  location                 : Location.t
}
and stmt_jsligo_content = statement_jsligo_content
and stmt_jsligo         = statement_jsligo
  [@@deriving yojson]

and switch_case =
| Switch_case          of (expr * statement_jsligo nseq option)
| Switch_default_case  of statement_jsligo nseq option

and switch = {
  switch_expr  : expr;
  switch_cases : switch_case nseq;
}

and namespace_statement_jsligo = {
  module_name        : string;
  namespace_content  : statement_jsligo nseq;
}

and import =
| Import_rename of {
  alias       : string;
  module_path : string nseq;
}
| Import_all_as of {
  alias       : string;
  module_str  : string;
}
| Import_selected of {
  imported    : string nseq;
  module_str  : string;
}

and while_stmt_jsligo = {
  expr        : expr;
  while_body  : statement_jsligo;
}

and index_kind = Let | Const

and for_of = {
  index_kind : index_kind;
  index      : string;
  expr       : expr;
  for_stmt   : statement_jsligo;
}

and statement_jsligo_content =
| S_Block      of statement_jsligo nseq
| S_Expr       of expr
| S_Cond       of statement_jsligo cond
| S_Return     of expr option
| S_Let        of let_binding nseq
| S_Const      of let_binding nseq
| S_Type       of type_decl
| S_Switch     of switch (* TODO : Use [case] record instead ? *)
| S_Break
| S_Namespace  of namespace_statement_jsligo
| S_Export     of statement_jsligo
| S_Import     of import
| S_While      of while_stmt_jsligo
| S_ForOf      of for_of
| S_Attrjs     of Attribute.t * statement_jsligo 

(* ========================== DECLARATIONS ================================= *)

and declaration = {
  declaration_content : declaration_content;
  location            : Location.t;
}
and decl_content = declaration_content
and decl         = declaration
  [@@deriving yojson]

and param_decl = {
  param_kind : [`Var | `Const];
  pattern    : pattern;
  param_type : type_expression option
}

and fun_decl = {
  is_rec      : bool;
  fun_name    : string;
  type_params : string nseq option;
  parameters  : param_decl list;
  ret_type    : type_expression option;
  return      : expression;
}

and type_decl = {
  name        : string;
  params      : string nseq option;
  type_expr   : type_expr;
}

and module_decl = {
  name     : string;
  mod_expr : module_;
}

and module_alias = {
  alias   : string;
  binders : string nseq;
}

and declaration_content =
| D_Directive      of Directive.t
| D_Attr           of (Attribute.t * declaration)
| D_ToplevelJsligo of statement_jsligo
| D_Let            of let_binding
| D_Const          of let_binding
| D_Type           of type_decl
| D_Module         of module_decl
| D_ModuleAlias    of module_alias
| D_Fun            of fun_decl

(* ========================== MODULES ====================================== *)

and module_ = {
  module_content : module_content;
  location       : Location.t;
}

and mod_ = module_
  [@@deriving yojson]

and module_content =
| M_Body of declaration nseq
| M_Path of string module_path
| M_Var  of string

(* ========================== EXPRESSIONS ================================== *)

and expression = {
  expression_content : expression_content;
  location           : Location.t
}
and expr_content = expression_content
and expr         = expression
  [@@deriving yojson]

and rev_app = {
  x : expr;
  f : expr;
}

and 'a selection =
| FieldName of string
| Component of 'a

and projection = {
  expr            : expr;
  field_path      : Z.t selection nseq;
}

and projection_jsligo = {
  expr            : expr;
  selection       : expr selection;
}

and path =
| Name of string
| Path of projection

and update_cameligo = {
  record_path  : path;
  updates : (path, expr) field nseq;
}
and update_pascaligo = {
  structure : expr;
  update    : expr; (* This expression should be a record *)
}

and updates = {
  record   : expr;
  updates  : (path, expr) field nseq;
}

and update = {
  record   : expr;
  path     : expr Module_access.t nseq;
  update   : expr;
}

and fun_expr_cameligo = {
  type_params  : Type_var.t nseq option;
  binders      : pattern nseq;
  rhs_type     : type_expr option;
  body         : expr;
}

and fun_expr_pascaligo = {
  type_params : Type_var.t nseq option;
  parameters  : param_decl list;
  ret_type    : type_expr option;
  return      : expr;
}

and body_jsligo =
| FunctionBody   of statement_jsligo nseq
| ExpressionBody of expr

and fun_expr_jsligo = {
  parameters : expr;
  lhs_type   : type_expr option;
  body       : body_jsligo;
}

and map_lookup = {
  map  : expr;
  keys : expr nseq;
}

and let_in = {
  is_rec       : bool;
  type_params  : Type_var.t nseq option;
  binders      : pattern nseq;
  rhs_type     : type_expr option;
  let_rhs      : expr;
  body         : expr;
}

and type_in = {
  type_binder : string;
  rhs         : type_expr;
  body        : expr;
}

and mod_in = {
  module_name : string;
  rhs         : module_;
  body        : expr;
}

and mod_alias = {
  module_name : string;
  binders     : string nseq;
  body        : expr;
}

and raw_code = {
  language    : string;
  code        : expression; (* Typically EAnnot( EString (raw code), TFun(signature) ) *)
}

and block_with_pascaligo = {
  block    : block_pascaligo;
  expr     : expr
}

and array_item_jsligo =
| Expr_entry of expr
| Rest_entry of expr

and array_jsligo = array_item_jsligo list

and property_jsligo =
| Punned_property of expr
| Property        of expr * expr
| Property_rest   of expr

and object_jsligo = property_jsligo nseq

and assignment_operator_jsligo =
  Times_eq
| Div_eq
| Min_eq
| Plus_eq
| Mod_eq

and operator_jsligo =
  Eq
| Assignment_operator of assignment_operator_jsligo

and assign_jsligo = {
  expr1 : expression;
  op    : operator_jsligo;
  expr2 : expression;
}

and ternary = {
  test   : expr;
  truthy : expr;
  falsy  : expr;
}

and expression_content = 

  (* Base *)
  | E_Literal  of Literal_value.t         (* 42, 10tez *)
  | E_Constant of constant                (* Cons hd tl) or (plus i j) *)
  | E_Par      of expr                    (* ( my_expression ) *)

  (* Variables *)
  | E_variable of Value_var.t             (* x *)

  (* Strings *)
  | E_Cat      of expr * expr             (* "hello" ^ "world" *)
  | E_String   of string                  (* "hello" *)
  | E_Verbatim of string                  (* {| hello |} *)

  (* Function calls *)
  | E_Call       of expr * expr nseq      (* f x y , calling f with arguments x and y *)

  (* Custom operators on functions *)
  | E_RevApp of rev_app                   (* x |> f *)

  (* Data structures *)
  | E_Tuple  of expr nseq                 (* (x, y, z) *)
  | E_RecordPascaligo of (expr,   expr) field list (* { x = 10; y; z } *)
  | E_RecordCameligo  of (string, expr) field nseq
  | E_ArrayJsligo of array_jsligo         (* [1, 2, 3] or [42] or even [] *)
  | E_Array  of expr list                 (* [1, 2, 3] or [42] or even [] *)

  | E_ObjectJsligo of object_jsligo       (* {a : 1, b : 2} *)

  (* Projections *)
  | E_Proj       of projection            (* x.y.1   y is a field name, 1 is a tuple component *)
  | E_ProjJsligo of projection_jsligo     (* x.y.z   Nested version of E_Proj, with only 1 selector *)

  (* Module access *)                     (* M.N.a *)
  | E_ModA    of expr module_access       (* nested version, E_ModA( M, E_ModA( N, E_Var var ) ) *)
  | E_ModPath of expr module_path         (* flat version,   E_ModAccess { [M, N], E_var var } *)

  (* Updates *)
  | E_UpdateCameligo  of update_cameligo  (* { my_record with field1 = a; field2 = b } *)
  | E_UpdatePascaligo of update_pascaligo (* myrec with record [field1 = 1; field2 = b] *)
  | E_Updates         of updates          (* Record update (flat version) *)
  | E_Update          of update           (* Record update (nested version) *)

  | E_FunCameligo  of fun_expr_cameligo    (* (fun (x, y) z -> x + y - z) *)
  | E_FunPascaligo of fun_expr_pascaligo
  | E_FunJsligo    of fun_expr_jsligo

  | E_Constr of (string * expr option)    (* let x = MyCtor 42 *)
  | E_App of (expr * expr nseq option)    (* MyCtor (42, 43, 44), PascaLigo only *)

  | E_Case of expr case                   (* match e with | A -> ... | B -> ... *)

  (* Type annotation *)
  | E_Annot       of (expr * type_expr)   (* 42 : int *)

  (* Conditionals *)
  | E_Cond of expr cond                   (* if b then 42 else 24 *)

  (* Lists *)
  | E_List of expr list                   (* [ 1; 2; 3; 4; 5] *)
  | E_Cons of (expr * expr)               (* head :: tail *)

  (* Sets *)
  | E_Set of expr list                    (* set [x; 1] *)

  (* Map lookup *)
  | E_MapLookup of map_lookup             (* M.m [i] *)

  (* Maps *)                              (* map [ "x" -> 1; "y" -> 2 ] *)
  | E_Map    of (expr * expr) list
  | E_BigMap of (expr * expr) list

  (* Let in *)
  | E_Let_in of let_in                    (* let x = 42 in x + 1 *)
  | E_TypeIn        of type_in            (* type t = int in let x : t = 42 *)
  | E_ModIn         of mod_in             (* module M = struct let x = 42 end in M.x *)
  | E_ModAlias      of mod_alias          (* module M = N.P in M.x *) 

  (* Code injection *)
  | E_RawCode of raw_code                 (* [%Michelson ({|...|} : nat -> nat) ] *)

  (* Sequences *)                         (* begin A; B; C end *)
  | E_Seq      of expr list               (* flat version : E_Seq [A; B; C] *)
  | E_Sequence of (expr * expr)           (* nested version : E_Sequence (A, E_Sequence(B, C)) *)

  (* Block *)                             (* function f ... is { const res = a + b; } with res *)
  | E_BlockPascaligo of block_with_pascaligo

  (* Attributes *)
  | E_Attr of (Attribute.t * expr)     (* [@a] (x,y)      *)

  (* Assign jsligo *)
  | E_AssignJsligo of assign_jsligo
  
  (* Ternary operator *)
  | E_Ternary of ternary                  (* x > 0 ? 42 : 24 *)

and constant =
  { cons_name: Constant.rich_constant (* this is at the end because it is huge *)
  ; arguments: expression list }

(* ========================== PROGRAM ====================================== *)

type program = declaration list
  [@@deriving yojson]
(* let program_to_yojson : program -> Yojson.Safe.t = fun _p -> `String "DUMMY" *)

(* ========================== NANOPASS TODO ================================ *)

