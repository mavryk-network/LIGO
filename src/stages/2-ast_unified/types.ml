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

type 'a nseq = 'a Simple_utils.List.Ne.t [@@deriving yojson, map]

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

type 'self type_expression_ = 'self type_expression_content_ Location.wrap
and 'self ty_expr_ = 'self type_expression_

and 'self type_expression_content_ =
  | T_Var of Ty_variable.t
  | T_Prod of 'self Prod.t
  | T_App of ('self, 'self) Type_app.t
  | T_Fun of 'self Arrow.t
  | T_Named_fun of 'self Named_fun.t
  | T_String of string
  | T_Int of string * Z.t
  | T_ModA of (Mod_variable.t, 'self) Mod_access.t
  | T_ModPath of (Mod_variable.t Simple_utils.List.Ne.t, 'self) Mod_access.t
  | T_Arg of string
  | T_Sum_raw of 'self option Non_linear_rows.t
  | T_Arg_sum_raw of 'self list option Non_linear_rows.t
    (* "curried" sum-type ["Ctor", arg1, arg2, arg3 ]*)
  | T_Record_raw of 'self option Non_linear_rows.t
  | T_Disc_union of 'self Non_linear_disc_rows.t
  | T_Attr of Attribute.t * 'self
[@@deriving map, yojson]

(* ========================== PATTERNS ===================================== *)
type ('self, 'ty_expr) pattern_ = ('self, 'ty_expr) pattern_content_ Location.wrap

and 'self list_pattern =
  | Cons of 'self * 'self
  | List of 'self list

and ('self, 'ty_expr) pattern_content_ =
  | P_unit
  | P_typed of 'ty_expr * 'self
  | P_literal of Literal_value.t
  | P_var of Variable.t
  | P_list of 'self list_pattern
  | P_variant of Label.t * 'self option
  | P_tuple of 'self list
  | P_pun_record of (Label.t, 'self) Field.t list
  | P_rest of Label.t
  | P_attr of Attribute.t * 'self
  | P_mod_access of (Mod_variable.t nseq, 'self) Mod_access.t
[@@deriving map, yojson]

(* ========================== INSTRUCTIONS ================================= *)
type ('self, 'expr, 'pattern, 'statement) instruction_ =
  ('self, 'expr, 'pattern, 'statement) instruction_content_ Location.wrap

and ('self, 'expr, 'pattern, 'statement) instruction_content_ =
  | I_struct_assign of 'expr Struct_assign.t
  | I_Call of 'expr Instruction_call.t
  | I_Case of ('expr, 'pattern, ('self, 'statement) Test_clause.t) Case.t
  | I_Cond of ('expr, ('self, 'statement) Test_clause.t) Cond.t
  | I_For of ('expr, 'statement) For_int.t
  | I_ForIn of ('expr, 'statement) For_collection.t
  | I_ForOf of ('expr, 'statement) For_of.t
  | I_Patch of 'expr Patch.t
  | I_Remove of 'expr Removal.t
  | I_Skip
  | I_While of ('expr, 'statement) While.t
  | I_Block of 'statement Simple_utils.List.Ne.t
  | I_Expr of 'expr
  | I_Return of 'expr option
  | I_Switch of ('expr, 'statement) Switch.t
  | I_break
[@@deriving map, yojson]

(* ========================== STATEMENTS ========================= *)
type ('self, 'instruction, 'declaration) statement_ =
  ('self, 'instruction, 'declaration) statement_content_ Location.wrap

and ('self, 'instruction, 'declaration) statement_content_ =
  | S_Attr of (Attribute.t * 'self)
  | S_Instr of 'instruction
  | S_Decl of 'declaration
[@@deriving map, yojson]
(* and stmt = statement [@@deriving yojson] *)

(* ========================== DECLARATIONS ================================= *)
type ('self, 'expr, 'ty_expr, 'pattern, 'mod_expr) declaration_ =
  ('self, 'expr, 'ty_expr, 'pattern, 'mod_expr) declaration_content_ Location.wrap

(* and decl_content = declaration_content *)
(* and decl = declaration [@@deriving yojson] *)
and ('self, 'expr, 'ty_expr, 'pattern, 'mod_expr) declaration_content_ =
  | D_Directive of Directive.t
  | D_Attr of (Attribute.t * 'self)
  | D_Import of Import.t
  | D_Export of 'self
  | D_Let of ('expr, 'pattern nseq, 'ty_expr) Let_decl.t
    (* let x = <..> ; let x (type a b) y (z:ty) = <..> *)
  | D_Var of ('pattern, 'expr, 'ty_expr) Simple_decl.t (* var x = y *)
  | D_Multi_var of ('pattern, 'expr, 'ty_expr) Simple_decl.t nseq (* var x = y , z = w*)
  | D_Const of ('pattern, 'expr, 'ty_expr) Simple_decl.t (* const x = y *)
  | D_Multi_const of
      ('pattern, 'expr, 'ty_expr) Simple_decl.t nseq (* const x = y , z = w *)
  | D_Fun of ('ty_expr, 'expr, ('pattern, 'ty_expr) Param.t) Fun_decl.t
  | D_Type of 'ty_expr Type_decl.t
  | D_Module of 'mod_expr Mod_decl.t
[@@deriving map, yojson]

(* ========================== MODULES ====================================== *)
include struct
  [@@@warning "-27"]

  type ('self, 'statement, 'declaration) mod_expr_ =
    ('self, 'statement, 'declaration) mod_expr_content_ Location.wrap

  and ('self, 'statement, 'declaration) mod_expr_content_ =
    | M_Body_statements of 'statement nseq
    | M_Body of 'declaration nseq
    | M_Path of Ligo_prim.Module_var.t nseq
    | M_Var of Ligo_prim.Module_var.t
  [@@deriving map, yojson]
end

(* ========================== EXPRESSIONS ================================== *)
type ('self, 'ty_expr, 'pattern, 'statement, 'mod_expr) expression_ =
  ('self, 'ty_expr, 'pattern, 'statement, 'mod_expr) expression_content_ Location.wrap

and ('self, 'ty_expr, 'pattern, 'statement, 'mod_expr) expr_ =
  ('self, 'ty_expr, 'pattern, 'statement, 'mod_expr) expression_

and ('self, 'ty_expr, 'pattern, 'statement, 'mod_expr) expression_content_ =
  | E_Attr of (Attribute.t * 'self) (* [@a] (x,y)      *)
  | E_Literal of Literal_value.t (* 42, 10tez *)
  | E_Binary_op of 'self Operators.binary_op
  | E_Unary_op of 'self Operators.unary_op
  | E_variable of Variable.t (* x *)
  | E_RevApp of 'self Rev_app.t (* x |> f *)
  | E_Tuple of 'self nseq (* (x, y, z) *)
  | E_Record_pun of (Variable.t, 'self) Field.t list (* { x = 10; y; z } *)
  | E_Array of
      'self Array_repr.t (* [1, 2, 3] , [42] , [] , [2 ...3] (specific to jsligo) *)
  | E_Object of 'self Object_.t (* {a : 1, b : 2} *)
  | E_List of 'self list (* [ 1; 2; 3; 4; 5] *)
  | E_Proj of 'self Projection.t (* x.y.1   y is a field name, 1 is a tuple component *)
  | E_ModA of (string, 'self) Mod_access.t (* M.N.a *)
  | E_ModPath of (string nseq, 'self) Mod_access.t
    (* nested version, E_ModA( M, E_ModA( N, E_Var var ) ) *)
  | E_Update of 'self Update.t
  | E_Poly_fun of
      ('self, 'ty_expr, 'pattern) Poly_fun.t (* (fun <type a b>(x, y) z -> x + y - z) *)
  | E_Block_fun of ('self, 'ty_expr, 'statement) Block_fun.t
  | E_Constr of 'self option Constructor.t (* let x = MyCtor 42 *)
  | E_App of ('self * 'self nseq option) (* MyCtor (42, 43, 44), PascaLigo only *)
  | E_Call of 'self * 'self nseq (* f (x, y) ; f x y *)
  | E_Case of ('self, 'pattern, 'self) Case.t (* match e with | A -> ... | B -> ... *)
  | E_Annot of ('self * 'ty_expr) (* 42 : int *)
  | E_Cond of ('self, 'self) Cond.t (* if b then 42 else 24 *)
  | E_Set of 'self list (* set [x; 1] *)
  | E_MapLookup of 'self Map_lookup.t
  | E_Map of ('self * 'self) list
  | E_BigMap of ('self * 'self) list
  | E_Let_in of ('pattern, 'self, 'ty_expr) Let_binding.t (* let x = 42 in x + 1 *)
  | E_TypeIn of ('self, 'ty_expr) Type_in.t (* type t = int in let x : t = 42 *)
  | E_ModIn of ('self, 'mod_expr) Mod_in.t (* module M = struct let x = 42 end in M.x *)
  | E_RawCode of 'self Raw_code.t
  | E_Sequence of ('self * 'self)
  | E_Block_with of ('self, 'statement) Block_with.t
  | E_AssignJsligo of 'self Assign_jsligo.t (* this is a very weird one .. *)
[@@deriving map, yojson]
(* ========================== PROGRAM ====================================== *)

type ('self, 'declaration, 'instruction) program_entry_ =
  | P_Attr of Attribute.t * 'self
  | P_Declaration of 'declaration
  | P_Top_level_instruction of 'instruction
  | P_Directive of Directive.t
(* and ('self, 'declaration, 'instruction) program_ =
  ('self, 'declaration, 'instruction) program_entry_ list *)
[@@deriving map, yojson]

(* fixpoints *)

type ty_expr = { fp : ty_expr ty_expr_ }
and pattern = { fp : (pattern, ty_expr) pattern_ }
and instruction = { fp : (instruction, expr, pattern, statement) instruction_ }
and statement = { fp : (statement, instruction, declaration) statement_ }
and declaration = { fp : (declaration, expr, ty_expr, pattern, mod_expr) declaration_ }
and mod_expr = { fp : (mod_expr, statement, declaration) mod_expr_ }
and expr = { fp : (expr, ty_expr, pattern, statement, mod_expr) expr_ }
and program_entry = { fp : (program_entry, declaration, instruction) program_entry_ }

(* one might wonder why ? go check `compile_toplevel_statement` unification of jsligo *)
type program = program_entry list

(* catamorphisms *)

let rec cata_expr
    ~(f_expr : (_, _, _, _, _) expr_ -> expr)
    ~(f_ty_expr : _ ty_expr_ -> ty_expr)
    ~(f_pattern : (_, _) pattern_ -> pattern)
    ~(f_statement : (_, _, _) statement_ -> statement)
    ~(f_mod_expr : (_, _, _) mod_expr_ -> mod_expr)
    ~(f_instruction : (_, _, _, _) instruction_ -> instruction)
    ~(f_declaration : (_, _, _, _, _) declaration_ -> declaration)
    (x : expr)
    : expr
  =
  let self =
    cata_expr
      ~f_expr
      ~f_ty_expr
      ~f_pattern
      ~f_statement
      ~f_mod_expr
      ~f_instruction
      ~f_declaration
  in
  let rec cata_ty_expr (x : ty_expr) : ty_expr =
    f_ty_expr (map_ty_expr_ cata_ty_expr x.fp)
  and cata_pattern (x : pattern) : pattern =
    f_pattern (map_pattern_ cata_pattern cata_ty_expr x.fp)
  and cata_instruction (x : instruction) : instruction =
    f_instruction
      (map_instruction_ cata_instruction self cata_pattern cata_statement x.fp)
  and cata_statement (x : statement) : statement =
    f_statement (map_statement_ cata_statement cata_instruction cata_declaration x.fp)
  and cata_declaration (x : declaration) : declaration =
    f_declaration
      (map_declaration_
         cata_declaration
         self
         cata_ty_expr
         cata_pattern
         cata_mod_expr
         x.fp)
  and cata_mod_expr (x : mod_expr) : mod_expr =
    f_mod_expr (map_mod_expr_ cata_mod_expr cata_statement cata_declaration x.fp)
  in
  f_expr (map_expr_ self cata_ty_expr cata_pattern cata_statement cata_mod_expr x.fp)


let rec cata_program_entry
    ~(f_program : (_, _, _) program_entry_ -> program_entry)
    ~(f_expr : (_, _, _, _, _) expr_ -> expr)
    ~(f_ty_expr : _ ty_expr_ -> ty_expr)
    ~(f_pattern : (_, _) pattern_ -> pattern)
    ~(f_statement : (_, _, _) statement_ -> statement)
    ~(f_mod_expr : (_, _, _) mod_expr_ -> mod_expr)
    ~(f_instruction : (_, _, _, _) instruction_ -> instruction)
    ~(f_declaration : (_, _, _, _, _) declaration_ -> declaration)
    (x : program_entry)
    : program_entry
  =
  let self =
    cata_program_entry
      ~f_program
      ~f_expr
      ~f_ty_expr
      ~f_pattern
      ~f_statement
      ~f_mod_expr
      ~f_instruction
      ~f_declaration
  in
  let rec cata_ty_expr (x : ty_expr) : ty_expr =
    f_ty_expr (map_ty_expr_ cata_ty_expr x.fp)
  and cata_expr (x : expr) : expr =
    f_expr
      (map_expr_ cata_expr cata_ty_expr cata_pattern cata_statement cata_mod_expr x.fp)
  and cata_pattern (x : pattern) : pattern =
    f_pattern (map_pattern_ cata_pattern cata_ty_expr x.fp)
  and cata_instruction (x : instruction) : instruction =
    f_instruction
      (map_instruction_ cata_instruction cata_expr cata_pattern cata_statement x.fp)
  and cata_statement (x : statement) : statement =
    f_statement (map_statement_ cata_statement cata_instruction cata_declaration x.fp)
  and cata_declaration (x : declaration) : declaration =
    f_declaration
      (map_declaration_
         cata_declaration
         cata_expr
         cata_ty_expr
         cata_pattern
         cata_mod_expr
         x.fp)
  and cata_mod_expr (x : mod_expr) : mod_expr =
    f_mod_expr (map_mod_expr_ cata_mod_expr cata_statement cata_declaration x.fp)
  in
  f_program (map_program_entry_ self cata_declaration cata_instruction x.fp)


let rec cata_program
    ~(f_program : (_, _, _) program_entry_ -> program_entry)
    ~(f_expr : (_, _, _, _, _) expr_ -> expr)
    ~(f_ty_expr : _ ty_expr_ -> ty_expr)
    ~(f_pattern : (_, _) pattern_ -> pattern)
    ~(f_statement : (_, _, _) statement_ -> statement)
    ~(f_mod_expr : (_, _, _) mod_expr_ -> mod_expr)
    ~(f_instruction : (_, _, _, _) instruction_ -> instruction)
    ~(f_declaration : (_, _, _, _, _) declaration_ -> declaration)
    (x : program)
    : program
  =
  List.map
    x
    ~f:
      (cata_program_entry
         ~f_program
         ~f_expr
         ~f_ty_expr
         ~f_pattern
         ~f_statement
         ~f_mod_expr
         ~f_instruction
         ~f_declaration)
