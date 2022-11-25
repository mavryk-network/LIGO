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

module Temp_prim = Temp_prim

module Location = Simple_utils.Location
module List = Simple_utils.List
module Label = Ligo_prim.Label
module Variable = Ligo_prim.Value_var
module Ty_variable = Ligo_prim.Type_var
module Mod_variable = Ligo_prim.Module_var
module Module_access = Ligo_prim.Module_access
module Literal_value = Ligo_prim.Literal_value
module Constant = Ligo_prim.Constant

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
module Named_fun = Temp_prim.Named_fun
module Prod = Temp_prim.Prod
module Type_app = Temp_prim.Type_app
module Arrow = Temp_prim.Arrow
module Mod_access = Temp_prim.Mod_access
module Struct_assign = Temp_prim.Struct_assign
module Instruction_call = Temp_prim.Instruction_call
module Case = Temp_prim.Case
module Test_clause = Temp_prim.Test_clause
module Cond = Temp_prim.Cond
module For_int = Temp_prim.For_int
module For_collection = Temp_prim.For_collection
module Patch = Temp_prim.Patch
module For_of = Temp_prim.For_of
module Removal = Temp_prim.Removal
module While = Temp_prim.While
module Switch = Temp_prim.Switch

module Z = Ligo_prim.Literal_value.Z

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


(* ========================== TYPES ======================================== *)

type type_expression = {
  type_expression_content : type_expression_content;
  location                : Location.t
}
and type_expr_content = type_expression_content
and type_expr         = type_expression
  [@@deriving yojson]

and type_expression_content =
| T_Var          of Ty_variable.t
| T_Prod         of type_expr Prod.t
| T_App          of (string,type_expr) Type_app.t
| T_Fun          of type_expr Arrow.t
| T_Named_fun    of type_expr Named_fun.t
| T_String       of string
| T_Int          of string * Z.t
| T_ModA         of (Mod_variable.t, type_expr) Mod_access.t
| T_ModPath      of (Mod_variable.t Simple_utils.List.Ne.t, type_expr) Mod_access.t
| T_Arg          of string
| T_Sum_raw      of type_expr option Non_linear_rows.t
| T_Arg_sum_raw  of type_expr list option Non_linear_rows.t (* "curried" sum-type ["Ctor", arg1, arg2, arg3 ]*)
| T_Record_raw   of type_expr option Non_linear_rows.t
| T_Disc_union   of type_expr Non_linear_disc_rows.t
| T_Attr         of Attribute.t * type_expr
| T_AppPascaligo of (type_expr, type_expr) Type_app.t

(* ========================== PATTERNS ===================================== *)

and ('lhs, 'rhs) field =
  | Punned of 'lhs
  | Complete of ('lhs * 'rhs)
  [@@deriving yojson]
and 'ty list_pattern =
  | Cons of 'ty p * 'ty p
  | List of 'ty p list
and 'ty pc =
  | P_unit
  | P_typed of 'ty * 'ty p
  | P_literal of Literal_value.t
  | P_var of Variable.t
  | P_list of 'ty list_pattern
  | P_variant of Label.t * 'ty p option
  | P_tuple of 'ty p list
  | P_pun_record of (Label.t, 'ty p) field list
  | P_rest of Label.t
  | P_attr of Attribute.t * 'ty p
  | P_mod_access of (Mod_variable.t nseq, 'ty p) Mod_access.t
and 'ty p = 'ty pc Location.wrap
and pattern_content = type_expr pc 
and pattern = type_expr p
  [@@deriving yojson]

(* ========================== INSTRUCTIONS ================================= *)
and instruction = {
  instruction_content : instruction_content;
  location                : Location.t
}
and instr_content = instruction_content
and instr         = instruction
  [@@deriving yojson]

and cond_branch = (instruction,statement) Test_clause.t

and instruction_content =
| I_struct_assign of expr Struct_assign.t
| I_Call   of expr Instruction_call.t
| I_Case   of (expr,pattern,cond_branch) Case.t
| I_Cond   of (expr,cond_branch) Cond.t
| I_For    of (expr,statement) For_int.t
| I_ForIn  of (expr,statement) For_collection.t
| I_ForOf  of (expr,statement) For_of.t
| I_Patch  of expr Patch.t
| I_Remove of expr Removal.t
| I_Skip
| I_While  of (expr,statement) While.t
| I_Block  of statement Simple_utils.List.Ne.t
| I_Expr   of expr
| I_Return of expr option
| I_Switch of (expr,statement) Switch.t
| I_break

(* ========================== STATEMENTS ========================= *)

and statement = {
  statement_content : statement_content;
  location                    : Location.t
}
and statement_content =
| S_Attr      of (Attribute.t * statement)
| S_Instr     of instruction
| S_Decl      of declaration
and stmt = statement
  [@@deriving yojson]


and var_decl = {
  pattern     : pattern;
  type_params : Ty_variable.t nseq option;
  var_type    : type_expression option;
  init        : expr;
}

and ('is_rec,'lhs,'body) let_binding = {
  is_rec      : 'is_rec;
  type_params : Ty_variable.t nseq option;
  pattern     : 'lhs;
  rhs_type   : type_expr option;
  let_rhs     : expression;
  body : 'body;
}

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
  type_params : Ty_variable.t nseq option;
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

and declaration_content =
| D_Directive      of Directive.t
| D_Attr           of (Attribute.t * declaration)
| D_Import         of import
| D_Export         of statement 
| D_Let            of (bool,pattern nseq,unit) let_binding
| D_Var            of (unit,pattern,unit) let_binding
| D_Multi_var      of (unit,pattern,unit) let_binding nseq
| D_Const          of (unit,pattern,unit) let_binding
| D_Multi_const    of (unit,pattern,unit) let_binding nseq
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
| M_Body_statements of statement nseq
| M_Body of declaration nseq
| M_Path of Mod_variable.t nseq
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
  type_params  : Ty_variable.t nseq option;
  binders      : pattern nseq;
  rhs_type     : type_expr option;
  body         : expr;
}

and fun_expr_pascaligo = {
  type_params : Ty_variable.t nseq option;
  parameters  : param_decl list;
  ret_type    : type_expr option;
  return      : expr;
}

and body_jsligo =
| FunctionBody   of statement nseq
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
  block    : statement nseq;
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
  | E_variable of Variable.t             (* x *)

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
  | E_ModA    of (string, expr) Mod_access.t (* nested version, E_ModA( M, E_ModA( N, E_Var var ) ) *)
  | E_ModPath of (string nseq, expr) Mod_access.t (* flat version,   E_ModAccess { [M, N], E_var var } *)

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

  | E_Case of (expr, pattern, expr) Case.t (* match e with | A -> ... | B -> ... *)

  (* Type annotation *)
  | E_Annot       of (expr * type_expr)   (* 42 : int *)

  (* Conditionals *)
  | E_Cond of (expr,expr) Cond.t          (* if b then 42 else 24 *)

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
  | E_Let_in of (bool,pattern nseq,expr) let_binding                    (* let x = 42 in x + 1 *)
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

type program_entry =
  | P_Attr of Attribute.t * program_entry
  | P_Declaration of declaration
  | P_Top_level_instruction of instruction
  | P_Directive of Directive.t
and program = program_entry list
  [@@deriving yojson]
