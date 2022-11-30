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
module Raw_code = Ligo_prim.Raw_code
module Constant = Ligo_prim.Constant
module Constructor = Ligo_prim.Constructor
module Non_linear_rows = Temp_prim.Non_linear_rows (Label)
module Field = Temp_prim.Field
module Array_repr = Temp_prim.Array_repr
module Object_ = Temp_prim.Object_
module Projection = Temp_prim.Projection
module Update = Temp_prim.Update
module Selection = Temp_prim.Selection
module Param = Temp_prim.Param
module Poly_fun = Temp_prim.Poly_fun
module Map_lookup = Temp_prim.Map_lookup
module Type_in = Temp_prim.Type_in
module Mod_in = Temp_prim.Mod_in
module Block_fun = Temp_prim.Block_fun
module Block_with = Temp_prim.Block_with
module Assign_jsligo = Temp_prim.Assign_jsligo


module Empty_label = struct
  type t = unit [@@deriving eq, compare, yojson, hash]

  let t_of_sexp _ = ()
  let sexp_of_t _ = failwith "wait"
  let of_string _ = ()
  let to_string () = "unlabeled"
end

module Non_linear_disc_rows = Temp_prim.Non_linear_rows (Empty_label)
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
module Import = Temp_prim.Import
module Let_decl = Temp_prim.Let_decl
module Simple_decl = Temp_prim.Simple_decl
module Fun_decl = Temp_prim.Fun_decl
module Type_decl = Temp_prim.Type_decl
module Mod_decl = Temp_prim.Mod_decl
module Operators = Temp_prim.Operators
module Let_binding = Temp_prim.Let_binding
module Rev_app = Temp_prim.Rev_app
module Z = Ligo_prim.Literal_value.Z

type 'a nseq = 'a Simple_utils.List.Ne.t [@@deriving yojson]

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

type type_expression =
  { type_expression_content : type_expression_content
  ; location : Location.t
  }

and type_expr_content = type_expression_content
and type_expr = type_expression [@@deriving yojson]

and type_expression_content =
  | T_Var of Ty_variable.t
  | T_Prod of type_expr Prod.t
  | T_App of (type_expr, type_expr) Type_app.t
  | T_Fun of type_expr Arrow.t
  | T_Named_fun of type_expr Named_fun.t
  | T_String of string
  | T_Int of string * Z.t
  | T_ModA of (Mod_variable.t, type_expr) Mod_access.t
  | T_ModPath of (Mod_variable.t Simple_utils.List.Ne.t, type_expr) Mod_access.t
  | T_Arg of string
  | T_Sum_raw of type_expr option Non_linear_rows.t
  | T_Arg_sum_raw of type_expr list option Non_linear_rows.t
    (* "curried" sum-type ["Ctor", arg1, arg2, arg3 ]*)
  | T_Record_raw of type_expr option Non_linear_rows.t
  | T_Disc_union of type_expr Non_linear_disc_rows.t
  | T_Attr of Attribute.t * type_expr

(* ========================== PATTERNS ===================================== *)

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
  | P_pun_record of (Label.t, 'ty p) Field.t list
  | P_rest of Label.t
  | P_attr of Attribute.t * 'ty p
  | P_mod_access of (Mod_variable.t nseq, 'ty p) Mod_access.t

and 'ty p = 'ty pc Location.wrap
and pattern_content = type_expr pc
and pattern = type_expr p [@@deriving yojson]

(* ========================== INSTRUCTIONS ================================= *)
and instruction =
  { instruction_content : instruction_content
  ; location : Location.t
  }

and instr_content = instruction_content
and instr = instruction [@@deriving yojson]
and cond_branch = (instruction, statement) Test_clause.t

and instruction_content =
  | I_struct_assign of expr Struct_assign.t
  | I_Call of expr Instruction_call.t
  | I_Case of (expr, pattern, cond_branch) Case.t
  | I_Cond of (expr, cond_branch) Cond.t
  | I_For of (expr, statement) For_int.t
  | I_ForIn of (expr, statement) For_collection.t
  | I_ForOf of (expr, statement) For_of.t
  | I_Patch of expr Patch.t
  | I_Remove of expr Removal.t
  | I_Skip
  | I_While of (expr, statement) While.t
  | I_Block of statement Simple_utils.List.Ne.t
  | I_Expr of expr
  | I_Return of expr option
  | I_Switch of (expr, statement) Switch.t
  | I_break

(* ========================== STATEMENTS ========================= *)
and statement =
  { statement_content : statement_content
  ; location : Location.t
  }

and statement_content =
  | S_Attr of (Attribute.t * statement)
  | S_Instr of instruction
  | S_Decl of declaration

and stmt = statement [@@deriving yojson]

(* ========================== DECLARATIONS ================================= *)
and declaration =
  { declaration_content : declaration_content
  ; location : Location.t
  }

and decl_content = declaration_content
and decl = declaration [@@deriving yojson]

and declaration_content =
  | D_Directive of Directive.t
  | D_Attr of (Attribute.t * declaration)
  | D_Import of Import.t
  | D_Export of declaration
  | D_Let of (expr, pattern nseq, type_expr) Let_decl.t
    (* let x = <..> ; let x (type a b) y (z:ty) = <..> *)
  | D_Var of (pattern, expr, type_expr) Simple_decl.t (* var x = y *)
  | D_Multi_var of (pattern, expr, type_expr) Simple_decl.t nseq (* var x = y , z = w*)
  | D_Const of (pattern, expr, type_expr) Simple_decl.t (* const x = y *)
  | D_Multi_const of
      (pattern, expr, type_expr) Simple_decl.t nseq (* const x = y , z = w *)
  | D_Fun of (type_expr, expr, (pattern,type_expr) Param.t) Fun_decl.t
  | D_Type of type_expr Type_decl.t
  | D_Module of module_ Mod_decl.t

(* ========================== MODULES ====================================== *)
and module_ =
  { module_content : module_content
  ; location : Location.t
  }
[@@deriving yojson]

and module_content =
  | M_Body_statements of statement nseq
  | M_Body of declaration nseq
  | M_Path of Ligo_prim.Module_var.t nseq
  | M_Var of Ligo_prim.Module_var.t

(* ========================== EXPRESSIONS ================================== *)
and expression =
  { expression_content : expression_content
  ; location : Location.t
  }

and expr_content = expression_content
and expr = expression [@@deriving yojson]

and expression_content =
  | E_Attr of (Attribute.t * expr) (* [@a] (x,y)      *)
  | E_Literal of Literal_value.t (* 42, 10tez *)
  | E_Binary_op of expr Operators.binary_op
  | E_Unary_op of expr Operators.unary_op
  | E_variable of Variable.t (* x *)
  | E_RevApp of expr Rev_app.t (* x |> f *)
  | E_Tuple of expr nseq (* (x, y, z) *)
  | E_Record_pun of (Variable.t, expr) Field.t list (* { x = 10; y; z } *)
  | E_Array of expr Array_repr.t (* [1, 2, 3] , [42] , [] , [2 ...3] (specific to jsligo) *)
  | E_Object of expr Object_.t (* {a : 1, b : 2} *)
  | E_List of expr list (* [ 1; 2; 3; 4; 5] *)
  | E_Proj of expr Projection.t (* x.y.1   y is a field name, 1 is a tuple component *)
  | E_ModA of (string, expr) Mod_access.t (* M.N.a *)
  | E_ModPath of (string nseq, expr) Mod_access.t (* nested version, E_ModA( M, E_ModA( N, E_Var var ) ) *)
  | E_Update of expr Update.t
  | E_Poly_fun of (expr,type_expr,pattern) Poly_fun.t (* (fun <type a b>(x, y) z -> x + y - z) *)
  | E_Block_fun of (expr,type_expr,stmt) Block_fun.t
  | E_Constr of expr option Constructor.t (* let x = MyCtor 42 *)
  | E_App of (expr * expr nseq option) (* MyCtor (42, 43, 44), PascaLigo only *)
  | E_Call of expr * expr nseq (* f (x, y) ; f x y *)
  | E_Case of (expr, pattern, expr) Case.t (* match e with | A -> ... | B -> ... *)
  | E_Annot of (expr * type_expr) (* 42 : int *)
  | E_Cond of (expr, expr) Cond.t (* if b then 42 else 24 *)
  | E_Set of expr list (* set [x; 1] *)
  | E_MapLookup of expr Map_lookup.t
  | E_Map of (expr * expr) list
  | E_BigMap of (expr * expr) list
  | E_Let_in of (pattern, expr, type_expr) Let_binding.t (* let x = 42 in x + 1 *)
  | E_TypeIn of (expr,type_expr) Type_in.t (* type t = int in let x : t = 42 *)
  | E_ModIn of (expr,module_) Mod_in.t (* module M = struct let x = 42 end in M.x *)
  | E_RawCode of expr Raw_code.t
  | E_Sequence of (expr * expr)
  | E_Block_with of (expr,stmt) Block_with.t
  | E_AssignJsligo of expr Assign_jsligo.t (* this is a very weird one .. *)

(* ========================== PROGRAM ====================================== *)

type program_entry =
  | P_Attr of Attribute.t * program_entry
  | P_Declaration of declaration
  | P_Top_level_instruction of instruction
  | P_Directive of Directive.t

and program = program_entry list [@@deriving yojson]
