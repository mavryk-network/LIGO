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
module Assign = Ligo_prim.Assign
module Assign_chainable = Temp_prim.Assign_chainable
module Type_decl = Temp_prim.Type_decl

(* Pattern_decl: to keep vs Ligo_prim (functor mess) *)
module Pattern_decl = Temp_prim.Pattern_decl
module Simple_let_in = Temp_prim.Simple_let_in
module Recursive = Temp_prim.Recursive
module Abstraction = Ligo_prim.Abstraction

module Empty_label = struct
  type t = unit [@@deriving eq, compare, yojson, iter, hash]

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
module Type_abstraction_decl = Temp_prim.Type_abstraction_decl
module Mod_decl = Temp_prim.Mod_decl
module Operators = Temp_prim.Operators
module Let_binding = Temp_prim.Let_binding
module Rev_app = Temp_prim.Rev_app
module Z = Ligo_prim.Literal_value.Z

(* The preprocessor directives are left unchanged during unification pass.
   So, the type of a directive is Preprocessor.Directive.t
   in both the CST and here in the AST unified.
   However, the Directive module is augmented here with
   yojson functions for the yojson ppx to work correctly. *)
module Directive = struct
  include Preprocessor.Directive [@@deriving sexp]

  let to_yojson : t -> Yojson.Safe.t =
   fun _ -> `String "JSON printing of directives is not supported"


  let dummy_directive = PP_Endif Region.ghost

  let of_yojson : Yojson.Safe.t -> (t, string) Result.t =
   fun _ -> Error "JSON parsing of directive is not supported"


  let sexp_of_t : t -> Sexp.t =
   fun _t -> Sexp.Atom "TODO: directive sexp generation is weird"


  let t_of_sexp : Sexp.t -> t =
   fun _sexp -> failwith "Directive parsing from sexp is not supported"
end
[@@deriving sexp]

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
  | T_Arg of string
  | T_Sum_raw of 'self option Non_linear_rows.t
  | T_Record_raw of 'self option Non_linear_rows.t
  | T_Disc_union of 'self Non_linear_disc_rows.t
  | T_Attr of Attribute.t * 'self
  (* \/ Below are nodes added through the passes \/ *)
  | T_Abstraction of 'self Abstraction.t [@not_initial]
  | T_Michelson_or of 'self * string * 'self * string [@not_initial]
  | T_Michelson_pair of 'self * string * 'self * string [@not_initial]
  | T_Sapling_state of string * Z.t [@not_initial]
  | T_Sapling_transaction of string * Z.t [@not_initial]
[@@deriving map, yojson, iter, sexp, is { tags = [ "not_initial" ]; name = "ty_expr" }]

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
  | P_mod_access of (Mod_variable.t Simple_utils.List.Ne.t, 'self) Mod_access.t
[@@deriving map, yojson, iter, sexp, is { tags = [ "not_initial" ]; name = "pattern" }]

(* ========================== INSTRUCTIONS ================================= *)
type ('self, 'expr, 'pattern, 'statement, 'block) instruction_ =
  ('self, 'expr, 'pattern, 'statement, 'block) instruction_content_ Location.wrap

and ('self, 'expr, 'pattern, 'statement, 'block) instruction_content_ =
  | I_Struct_assign of 'expr Struct_assign.t
  | I_Call of 'expr Instruction_call.t
  | I_Case of ('expr, 'pattern, ('self, 'block) Test_clause.t) Case.t
  | I_Cond of ('expr, ('self, 'block) Test_clause.t) Cond.t
  | I_For of ('expr, 'block) For_int.t
  | I_ForIn of ('pattern, 'expr, 'block) For_collection.t
  | I_ForOf of ('expr, 'statement) For_of.t
  | I_Patch of 'expr Patch.t
  | I_Remove of 'expr Removal.t
  | I_Skip
  | I_While of ('expr, 'block) While.t
  | I_Block of 'block
  | I_Expr of 'expr
  | I_Return of 'expr option [@sexp.option]
  | I_Switch of ('expr, 'block) Switch.t
  | I_break
  (*  \/ Below are nodes added through the passes \/*)
  | I_Assign of Variable.t * 'expr [@not_initial]
[@@deriving
  map, yojson, iter, sexp, is { tags = [ "not_initial" ]; name = "instruction" }]

(* ========================== STATEMENTS ========================= *)
type ('self, 'instruction, 'declaration) statement_ =
  ('self, 'instruction, 'declaration) statement_content_ Location.wrap

and ('self, 'instruction, 'declaration) statement_content_ =
  | S_Attr of (Attribute.t * 'self)
  | S_Instr of 'instruction
  | S_Decl of 'declaration
[@@deriving map, yojson, iter, sexp, is { tags = [ "not_initial" ]; name = "statement" }]
(* and stmt = statement [@@deriving yojson] *)

(* ========================== BLOCKS ======================================= *)

include struct
  [@@@warning "-27"]

  type ('self, 'statement) block_ = 'statement Simple_utils.List.Ne.t Location.wrap
  [@@deriving map, yojson, iter, sexp]
end

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
  | D_Let of ('expr, 'pattern Simple_utils.List.Ne.t, 'ty_expr) Let_decl.t
    (* let x = <..> ; let x (type a b) y (z:ty) = <..> *)
  | D_Var of ('pattern, 'expr, 'ty_expr) Simple_decl.t (* var x = y *)
  | D_Multi_var of ('pattern, 'expr, 'ty_expr) Simple_decl.t Simple_utils.List.Ne.t
    (* var x = y , z = w *)
  | D_Const of ('pattern, 'expr, 'ty_expr) Simple_decl.t (* const x = y *)
  | D_Multi_const of ('pattern, 'expr, 'ty_expr) Simple_decl.t Simple_utils.List.Ne.t
    (* const x = y , z = w *)
  | D_Fun of ('ty_expr, 'expr, ('pattern, 'ty_expr) Param.t) Fun_decl.t
  | D_Type_abstraction of 'ty_expr Type_abstraction_decl.t
  | D_Module of 'mod_expr Mod_decl.t
  (*  \/ Below are nodes added through the passes \/*)
  | D_Type of 'ty_expr Type_decl.t [@not_initial]
  | D_irrefutable_match of ('expr, 'pattern) Pattern_decl.t [@not_initial]
[@@deriving
  map, yojson, iter, sexp, is { tags = [ "not_initial" ]; name = "declaration" }]

(* ========================== MODULES ====================================== *)
include struct
  [@@@warning "-27"]

  type ('self, 'program_entry) mod_expr_ =
    ('self, 'program_entry) mod_expr_content_ Location.wrap

  and ('self, 'program_entry) mod_expr_content_ =
    | M_Body of 'program_entry Simple_utils.List.Ne.t
    | M_Path of Ligo_prim.Module_var.t Simple_utils.List.Ne.t
    | M_Var of Ligo_prim.Module_var.t
  [@@deriving map, iter, yojson, sexp, is { tags = [ "not_initial" ]; name = "mod_expr" }]
end

(* ========================== EXPRESSIONS ================================== *)
type ('self, 'ty_expr, 'pattern, 'block, 'mod_expr) expression_ =
  ('self, 'ty_expr, 'pattern, 'block, 'mod_expr) expression_content_ Location.wrap

and ('self, 'ty_expr, 'pattern, 'block, 'mod_expr) expr_ =
  ('self, 'ty_expr, 'pattern, 'block, 'mod_expr) expression_

and ('self, 'ty_expr, 'pattern, 'block, 'mod_expr) expression_content_ =
  | E_Attr of (Attribute.t * 'self) (* [@a] (x,y)      *)
  | E_Literal of Literal_value.t (* 42, 10tez *)
  | E_Binary_op of 'self Operators.binary_op
  | E_Unary_op of 'self Operators.unary_op
  | E_variable of Variable.t (* x *)
  | E_RevApp of 'self Rev_app.t (* x |> f *)
  | E_Tuple of 'self Simple_utils.List.Ne.t (* (x, y, z) *)
  | E_Record_pun of (Variable.t, 'self) Field.t list (* { x = 10; y; z } *)
  | E_Array of
      'self Array_repr.t (* [1, 2, 3] , [42] , [] , [2 ...3] (specific to jsligo) *)
  | E_Object of 'self Object_.t (* {a : 1, b : 2}  ; { a ... n } *)
  | E_List of 'self list (* [ 1; 2; 3; 4; 5] *)
  | E_Proj of 'self Projection.t (* x.y.1   y is a field name, 1 is a tuple component *)
  | E_ModA of (string, 'self) Mod_access.t (* M.N.a *)
  | E_Update of 'self Update.t
  | E_Poly_fun of
      ('self, 'ty_expr, 'pattern) Poly_fun.t (* (fun <type a b>(x, y) z -> x + y - z) *)
  | E_Block_fun of ('self, 'pattern, 'ty_expr, 'block) Block_fun.t
  | E_Constr of Label.t
  | E_Ctor_App of ('self * 'self Simple_utils.List.Ne.t option)
    (* MyCtor (42, 43, 44), PascaLigo only *)
  | E_Call of 'self * 'self list (* f (x, y) ; f x y *)
  | E_Match of ('self, 'pattern, 'self) Case.t (* match e with | A -> ... | B -> ... *)
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
  | E_Block_with of ('self, 'block) Block_with.t (* { tata ; toto } with whatev *)
  | E_Assign_chainable of
      'self Assign_chainable.t (* x := y ; which has the type of x/y *)
  | E_Let_mut_in of ('pattern, 'self, 'ty_expr) Let_binding.t (* let mut x = 1 *)
  | E_Assign_unitary of
      ('self, 'ty_expr option) Assign.t (* x := y ; which has type unit *)
  | E_While of ('self, 'self) While.t
  | E_For of ('self, 'self) For_int.t
  | E_For_in of ('pattern, 'self, 'self) For_collection.t
  | E_Sequence of 'self list (* begin a ; a () ; x := y end *)
  (*  \/ Below are nodes added through the passes \/ *)
  | E_constant of 'self Constant.t [@not_initial]
  | E_Constructor of 'self Constructor.t [@not_initial]
  | E_Simple_let_in of ('self, 'pattern) Simple_let_in.t [@not_initial]
  | E_Poly_recursive of ('self, unit, ('self, 'ty_expr, 'pattern) Poly_fun.t) Recursive.t
      [@not_initial]
[@@deriving map, iter, yojson, sexp, is { tags = [ "not_initial" ]; name = "expr" }]
(* ========================== PROGRAM ====================================== *)

type ('self, 'declaration, 'instruction) program_entry_ =
  | PE_Attr of Attribute.t * 'self
  | PE_Declaration of 'declaration
  | PE_Top_level_instruction of 'instruction
  | PE_Preproc_directive of Directive.t
(*
  would like to write that, but it makes unification using (_,_,_) program_entry_
  and it was annoying, see type `program` bellow
*)
(* and ('self, 'declaration, 'instruction) program_ =
  ('self, 'declaration, 'instruction) program_entry_ list *)
[@@deriving
  map, yojson, iter, sexp, is { tags = [ "not_initial" ]; name = "program_entry" }]

type ty_expr = { fp : ty_expr ty_expr_ }
and pattern = { fp : (pattern, ty_expr) pattern_ }
and instruction = { fp : (instruction, expr, pattern, statement, block) instruction_ }
and statement = { fp : (statement, instruction, declaration) statement_ }
and block = { fp : (block, statement) block_ }
and declaration = { fp : (declaration, expr, ty_expr, pattern, mod_expr) declaration_ }
and mod_expr = { fp : (mod_expr, program_entry) mod_expr_ }
and expr = { fp : (expr, ty_expr, pattern, block, mod_expr) expr_ }
and program_entry = { fp : (program_entry, declaration, instruction) program_entry_ }

(* one might wonder why ? go check `compile_toplevel_statement` unification of jsligo *)
type program = program_entry list

(*
TODO:

have a type program = program_entry list

some nanopass (going from one statement/program_entry to multiple ones) will be slightly easier to write
*)