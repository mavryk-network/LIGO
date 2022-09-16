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

(* include Stage_common.Types *)
open Ligo_prim

module Location = Simple_utils.Location
module List = Simple_utils.List

module Z = Literal_value.Z

type 'a nseq = 'a Simple_utils.List.Ne.t
  [@@deriving yojson]

(* open Simple_utils.Utils *)

(* TODO NP : Copy/pasted from vendors/LexerLib, find a way to not copy/paste *)
module Directive = struct
  type linenum    = int [@@deriving yojson]
  type file_path  = string [@@deriving yojson]
  type flag       = Push | Pop [@@deriving yojson]
  type linemarker = linenum * file_path * flag option
  [@@deriving yojson]

  type t =
    Linemarker of linemarker
    [@@deriving yojson]

  type directive = t
  [@@deriving yojson]
end (* of module Directive *)

type attr_pascaligo = {
  key      : string;
  value    : string option;
} [@@deriving yojson]

(* ========================== TYPES ======================================== *)

type type_expression = {
  type_expression_content : type_expression_content;
  location                : Location.t
}
and type_expr_content = type_expression_content
and type_expr         = type_expression
  [@@deriving yojson]

and type_expression_content = T_Dummy  (* TODO NP : Add type expressions *)

(* ========================== PATTERNS ===================================== *)

and pattern = P_Dummy  (* TODO NP : Add pattern expressions *)

(* ========================== INSTRUCTIONS ================================= *)
type instruction = {
  instruction_content : instruction_content;
  location                : Location.t
}
and instr_content = instruction_content
and instr         = instruction
  [@@deriving yojson]

and instruction_content = I_Dummy  (* TODO NP : Add instructions *)

(* ========================== STATEMENTS =================================== *)

and statement = {
  statement_content : statement_content;
  location          : Location.t
}
and stmt_content = statement_content
and stmt         = statement
  [@@deriving yojson]

and var_decl = {
  pattern     : pattern;
  type_params : string nseq option;
  var_type    : type_expression option;
  init        : expr;
}

(* TODO NP : Merge with 'cond_expr' type ? *)
and cond_statement = {
  test     : expr;
  s_ifso   : statement;
  s_ifnot  : statement option;
}

(* TODO NP : Merge with 'case' record ? *)
and switch_case =
| Switch_case          of (expr * statement nseq option)
| Switch_default_case  of statement nseq option

and switch = {
  switch_expr  : expr;
  switch_cases : switch_case nseq;
}

and namespace_statement = {
  module_name        : string;
  namespace_content  : statement nseq;
}

(* TODO NP : Merge with type 'module_alias' ? *)
and import = {
  alias        : string;
  module_path  : string nseq;
}

and while_stmt = {
  expr        : expr;
  while_body  : statement;
}

and index_kind = Let | Const

and for_of = {
  index_kind : index_kind;
  index      : string;
  expr       : expr;
  for_stmt   : statement;
}

(* TODO NP : Separate statement into statement_pascaligo / statement_jsligo ? *)
and statement_content =
  (* Pascaligo *)
| S_Attr      of (attr_pascaligo * statement) 
| S_Decl      of declaration
| S_Instr     of instruction
| S_VarDecl   of var_decl
  (* Jsligo *)
| SBlock      of statement nseq
| SExpr       of expr
| SCond       of cond_statement
| SReturn     of expr option
| SLet        of let_binding nseq
| SConst      of let_binding nseq
| SType       of type_decl
| SSwitch     of switch
| SBreak
| SNamespace  of namespace_statement
| SExport     of statement
| SImport     of import
| SWhile      of while_stmt
| SForOf      of for_of

(* ========================== DECLARATIONS ================================= *)

and declaration = {
  declaration_content : declaration_content;
  location            : Location.t;
}
and decl_content = declaration_content
and decl         = declaration
  [@@deriving yojson]

and let_binding = {
  is_rec      : bool;
  type_params : string nseq option;
  binders     : pattern nseq;
  rhs_type    : type_expr option;
  let_rhs     : expression;
}

and param_decl = {
  param_kind : [`Var | `Const];
  pattern    : pattern;
  param_type : type_expression option
}

(* TODO NP : Merge this type with 'let_binding' ? *)
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
| D_Attr           of (attr_pascaligo * declaration)
| D_ToplevelJsligo of statement
| D_Let            of let_binding
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

and module_content = M_Dummy

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

and ('lhs, 'rhs) field =
  | Punned of 'lhs
  | Complete of ('lhs * 'rhs)

and field_assign = {
  name : string;
  expr : expr; 
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

and module_access = {
  module_name : string;
  field       : expr;
}

and module_path = {
  module_path : string nseq;
  field       : expr;
}

and path =
| Name of string
| Path of projection

and field_path_assignment = {
  field_path : path;
  field_expr : expr;
}

and update_cameligo = {
  record_path  : path;
  updates : field_path_assignment nseq;
}
and update_pascaligo = {
  structure : expr;
  update    : expr; (* This expression should be a record *)
}

and updates = {
  record   : expr;
  updates  : field_path_assignment nseq;
}

and update = {
  record   : expr;
  path     : expr Module_access.t nseq;
  update   : expr;
}

and fun_expr_cameligo = {
  type_params  : string nseq option;
  binders      : pattern nseq;
  rhs_type     : type_expr option;
  body         : expr;
}

and fun_expr_pascaligo = {
  type_params : string nseq option;
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

and case_clause = {
  pattern      : pattern;
  rhs          : expr;
}

and case = {
  expr         : expr;
  cases        : case_clause nseq;
}

and cond_expr = {
  test         : expr;
  ifso         : expr;
  ifnot        : expr option;
}

and map_lookup = {
  map  : expr;
  keys : expr nseq;
}

and let_in_cameligo = {
  is_rec       : bool;
  type_params  : string nseq option;
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

and block_with = {
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

and expression_content = 

  (* Base *)
  | E_Literal  of Literal_value.t         (* 42, 10tez *)
  | E_Constant of constant                (* Cons hd tl) or (plus i j) *)
  | E_Par      of expr                    (* ( my_expression ) *)

  (* Variables *)
  | E_UserVar  of string                  (* x            user-defined variable
                                             Tezos.self   pseudo-module constants  *)
  | E_variable of Value_var.t             (* user-defined or compiler-generated variables *)

  (* Strings *)
  | E_Cat      of expr * expr             (* "hello" ^ "world" *)
  | E_String   of string                  (* "hello" *)
  | E_Verbatim of string                  (* {| hello |} *)

  (* Function calls *)
  | E_Call       of expr * expr nseq      (* f x y , calling f with arguments x and y *)
  | E_CallJsligo of expr * expr nseq      (* Contains "special" calls like 'list' *)

  (* Custom operators on functions *)
  | E_RevApp of rev_app                   (* x |> f *)

  (* Data structures *)
  | E_Tuple  of expr nseq                 (* (x, y, z) *)
  | E_Rec    of (expr, expr) field list   (* Fields can be punned : { x = 10; y; z } *)
  | E_Record of field_assign nseq         (* Field are not punned : { x = 10; y = y } *)
  | E_ArrayJsligo of array_jsligo         (* [1, 2, 3] or [42] or even [] *)
  | E_Array  of expr list                 (* [1, 2, 3] or [42] or even [] *)

  | E_ObjectJsligo of object_jsligo       (* {a : 1, b : 2} *)

  (* Projections *)
  | E_Proj       of projection            (* x.y.1   y is a field name, 1 is a tuple component *)
  | E_ProjJsligo of projection_jsligo     (* x.y.z   Nested version of E_Proj, with only 1 selector *)

  (* Module access *)                     (* M.N.a *)
  | E_ModA    of module_access            (* nested version, E_ModA( M, E_ModA( N, E_Var var ) ) *)
  | E_ModPath of module_path              (* flat version,   E_ModAccess { [M, N], E_var var } *)

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

  | E_Case of case                        (* match e with | A -> ... | B -> ... *)

  (* Type annotation *)
  | E_Annot       of (expr * type_expr)   (* 42 : int *)
  | E_AnnotJsligo of (expr * type_expr)   (* Same as E_Annot, but some annots are special *)

  (* Conditionals *)
  | E_Cond of cond_expr                   (* if b then 42 else 24 *)

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
  | E_LetInCameligo of let_in_cameligo    (* let x = 42 in x + 1 *)
  | E_TypeIn        of type_in            (* type t = int in let x : t = 42 *)
  | E_ModIn         of mod_in             (* module M = struct let x = 42 end in M.x *)
  | E_ModAlias      of mod_alias          (* module M = N.P in M.x *) 

  (* Code injection *)
  | E_RawCode of raw_code                 (* [%Michelson ({|...|} : nat -> nat) ] *)

  (* Sequences *)                         (* begin A; B; C end *)
  | E_Seq      of expr list               (* flat version : E_Seq [A; B; C] *)
  | E_Sequence of (expr * expr)           (* nested version : E_Sequence (A, E_Sequence(B, C)) *)

  (* Block *)                             (* function f ... is { const res = a + b; } with res *)
  | E_Block of block_with

  (* Attributes (PascaLigo) *)
  | E_Attr of (attr_pascaligo * expr)     (* [@a] (x,y)      *)

  (* Assign jsligo *)
  | E_AssignJsligo of assign_jsligo

and constant =
  { cons_name: Constant.rich_constant (* this is at the end because it is huge *)
  ; arguments: expression list }

(* ========================== PROGRAM ====================================== *)

type program = declaration list (* TODO NP : Try to convert this into non-empty list (cannot use Trace.collect with non-empty lists) *)
  [@@deriving yojson]
(* let program_to_yojson : program -> Yojson.Safe.t = fun _p -> `String "DUMMY" *)

(* ========================== NANOPASS TODO ================================ *)

(*

  E_Variable / E_Constant
  =============================================================================
  pass 'constantize' (we should not need this (other MR))
    remove : E_UserVar
    new   : E_Variable | E_Constant

    rules :
    A. constantize @@ E_UserVar v where (    Constants.mem v) = E_Constant v
    B. constantize @@ E_UserVar v where (not Constants.mem v) = E_Variable v

    E_ctor ("Unit") |-> E_literal (Literal_unit) [ if syntax == pascaligo (?)]

  E_Par
  =============================================================================
  pass 'unpar' :
    remove: E_par

    (remove parenthesis, trivial in forward pass, useful in backward pass)
    rule :
    A. unpar @@ E_Par e = e

  E_String
  =============================================================================
  pass 'strings' :
    remove : E_String
           | E_Cat
           | E_Verbatim
    add    : E_Literal(Literal_string | Literal_verbatim)
           | E_constant( CONCAT )

  E_Call
  =============================================================================

  pass 'constantize_calls' :
    remove : E_Call { f = E_Constant c; args }
    add    : E_Constant c args
    needs  : pass 'constantize'
  
    rules :
    A. n_constantize_call @@ E_Call {var = E_Constant c; _} where is_constant s
      ===> becomes ===> E_constant s

    Transforms :
    E_Call {
      var : AST.expr =
        | E_Constant (c : string)  (case 1)
        | E_Var (v : string)       (case 2)
      args : AST.expr nseq
    } 
    into 
    | E_Constant (c : string)      (case 1) (rule A.)
      args : AST.expr nseq
    | E_Call {                              (rule B.)
        var : AST.expr =
          | E_Var (v : string)     (case 2)
        args : AST.expr nseq
      }


  pass 'constantize_module_calls' :
    need   : - flatten_modules
             - pseudo_module_accesses
             - constantize_calls
    remove : E_Call { f = E_ModPath; args } where built_ins func.module_name
                                              and constants func
    new : E_Call or exception

    This particular pass corresponds to the special case below in Cameligo :
      E_Call ({ value=( E_ModPath { value={ module_path = (module_name,[]) ; field ; _ }; region=_ }, args ); region } as call)
    Handle these two exception cases
    (handle pseudo-constants like Tezos.self for example)
    where the code does nothing but raise an exception
    on E_Call{func, args} when func is a pseudo-module
    and args match certain patterns

    E_Call
      E_ModPath
        module_name = "Tezos"
        field       = E_UserVar "self"
      args
    ======> becomes (with nanopass 'pseudo_module_accesses')
    E_Call
      E_Constant C_SELF
      args
    ======> must become (with this nanopass)
    E_Constant C_SELF args

  pass 'applications' :
  remove : E_Call
    new : E_application
    depends :
      - 'constantize' (We'll need to handle the pseudo-module constants first)
      - 'pseudo_module_calls'

    E_Call (func : AST.expr) (args : AST.expr list) -> E_Application
    
  E_Tuple
  =============================================================================
  pass 'tuple_singletons' :
    new : - 
    remove E_Tuple ( one_element_only )

    Remove singleton tuples
    E_Tuple ( E_xxx ) |-> E_xxx

  
  E_Record
  =============================================================================
  pass 'unpun' :  (PascaLigo)
    need : variabilize
    remove : E_Rec
    new : E_Record

    Unfold punned fields
    E_Rec (punned or complete fields) |-> E_Record (all complete fields)

    More precisely :
    E_Rec (expr_lhs, expr_rhs) |->
      match expr_lhs with
      | E_Variable str -> E_Record (name = str, expr_rhs)
      | E_Par inside   -> unpun (E_Rec (inside, expr_rhs))
      | _              -> failwith "Unexpected expression"
  
  
  E_RevApp
  =============================================================================
  pass 'e_rev_app' (CameLigo)
      remove : E_RevApp
      add    : E_Application

      E_RevApp(x, f) |-> E_Application(lamb=f, args=x)

  E_Proj
  =============================================================================
  pass 'proj_to_access'
    remove : E_Proj
    new : E_Accessor

    Transforms :
      expr projection =
      expr                  expr
      list                  field_path
        | string            FieldName         (case 1)
        | Z.t               Component         (case 2)
    Into :
      'expr accessor =
      'expr                 record
      list                  path
        |  z                Access_tuple      (case 2)
        |  string           Access_record     (case 1)
        |  'expr            Access_map
    
    Note : the [Access_map[] case will be used for [E_MapLookup] compilation.
    
  E_ModA / E_ModPath
  =============================================================================
  pass 'flatten_modules'
    remove : E_ModA
    add    : E_ModPath
  
    Replace nested module access of E_ModA by flat ones of E_ModPath
  
  pass 'pseudo_module_accesses'
    needs  : - flatten_modules
    remove : E_ModPath    (where the module path is in the 'built_ins')
    add    : E_Constant

    Transform every E_ModPath matching a built-in constant
    into the appropriate E_Constant

    for expression : Tezos.self
    Transform :
    E_ModA
      module_name =   "Tezos"
      field       =   E_UserVar "self"
    Into :
    E_Constant C_SELF

  pass 'modules_and_projections'
    need : - flatten_modules
           - proj_to_access
    remove : E_ModPath ( E_Proj () )
    add    : E_Proj ( E_ModPath () )

    The idea is to convert :
      M.N.(rec_or_tuple.x.1.y.2) |-> (M.N.rec_or_tuple).x.1.y.2

    In other words, transform :
    > E_ModPath
    >   module_path = ["M"; "N"]
    >   field =
    >     E_Proj
    >       expr = E_UserVar "rec_or_tuple"
    >       field_path = [
    >         FieldName", "x"
    >         Component", "1"
    >         FieldName", "y"
    >         Component", "2" 
    >       ]
    into
    > E_Proj
    >   expr =
    >     E_ModPath
    >       module_path = ["M"; "N"]
    >       field = E_UserVar "rec_or_tuple"
    >   field_path = [
    >     FieldName", "x"
    >     Component", "1"
    >     FieldName", "y"
    >     Component", "2" 
    >   ]


  E_Update
  =============================================================================
  pass : record_update_cameligo
    remove : E_UpdateCameligo
    new    : E_Updates

  pass : record_update_pascaligo
    needs  : - unpun
    remove : E_UpdatePascaligo
    new    : E_Updates
  
  pass : record_update_unflatten
    remove : E_Updates
    new    : E_Update

    The goal of this pass is to transform flat record updates into nested ones.
    For example :
    E_Updates ( my_record, [update1; update2; update3] )
    becomes
    E_Update (
      E_Update (
        E_Update ( my_record, udpate1),
        update2
      ),
      update3
    )

  E_Fun
  =============================================================================

  pass : check_annotations

    In the lhs pattern, check that every variable has a type annotation.
    (see the [check_annotation] function in pass 04-tree_abstraction)

  pass : functions_cameligo
    remove : E_FunCameligo
    add    : E_lambda

  E_App
  =============================================================================

  pass : ctor_pascaligo
    remove : E_App ( E_Ctor )
    add    : E_Ctor | failwith
  
    In CameLIGO and JsLIGO, MyCtor 42 becomes E_Ctor ( "MyCtor", 42 )
    In PascaLIGO, we have MyCtor (42) which becomes
    E_App ( E_Ctor "MyCtor", 42 )
    
    We want equivalent ctor to lead to the same AST, so we want to transform :
    * E_App ( E_Ctor ("MyCtor", ()), 42 )   |-> E_Ctor ( "MyCtor", 42 )

    Also, E_App is only used in PascaLigo in conjunction with E_Ctor, so :
    * E_App ( E_xxx != E_Ctor )             |-> failwith "Unexpected expression"

    Third, there is this case :
    * E_App ( E_Ctor "Unit", None)         |-> E_Unit
    This third case should be apply for PascaLigo only,
    so it should be applied to E_App only.
    If we use a later nanopass to transform E_Ctor "Unit" into E_Unit,
    the CameLigo's Unit constructor will also be compiled as E_Unit.

  E_List / E_Cons
  =============================================================================

  pass : e_cons
    remove : E_Cons
    add    : E_Constant ( C_CONS )
    
  E_LetIn
  =============================================================================
  pass : e_let_in_cameligo
    remove : E_LetInCameligo
    add    : E_LetIn
    
    Direct mapping E_LetInCameligo |-> E_LetIn

  pass : matching_let_in
      remove : E_LetIn with matching pattern
      add    : E_Matching

      Converting the E_LetIn with an irrefutable pattern into E_Matching

  E_ModIn / E_ModAlias
  =============================================================================
  pass : e_mod_in_cameligo
    remove : E_ModInCameligo
    add    : E_ModIn

    E_ModInCameligo is meant to be similar to the CST.Cameligo node.
    E_ModIn is meant to be identical to the AST Imperative one.
    There are a few transformations to do :
    - the module variable should be converted : string -> module_variable
    - the rhs too : module_ -> M_struct of ... : module_expr

    Actually, both E_ModInCameligo and E_ModAlias will be converted to E_ModIn,
    but the rhs will be :
    - M_Struct (for E_ModInCameligo)
    - M_Path (for E_ModAlias)
  
  pass : e_mod_alias
    remove : E_ModAlias
    add    : E_ModIn
  
  E_seq
  =============================================================================
  pass e_seq
    remove : E_Seq
    add : E_Sequence | E_Unit | expression

    E_Seq []        becomes  E_Unit
    E_Seq [e]       becomes  e
    E_Seq [e1; e2]  becomes  E_Sequence (e1, e2)

    E_Seq [e1; e2; ...; en-1; en]  becomes
    E_Sequence ( e1,
      E_Sequence ( e2,
        ... E_Sequence ( en-1, en)
      ) 
    )

  E_MapLookup
  =============================================================================
  pass 'map_lookup_to_access' (PascaLigo)
    remove : E_MapLookup
    new : E_Access

    Transforms :
      map_lookup =
      expr                  map
      nseq                  keys
        expr
    Into :
      'expr accessor =
      'expr                 record
      list                  path
        |  z                Access_tuple
        |  string           Access_record
        |  'expr            Access_map         (use this one)
    
    Note : Only use the [Access_map] constructor here,
    the two others are used by the [proj_to_access] nanopass, for [E_Proj]
    
  E_Map / E_BigMap
  =============================================================================
  pass 'map_to_map' (PascaLigo)
    remove : AST_U.E_Map | AST_U.E_BigMap
    add    : AST_I.E_Map | AST_I.E_BigMap

    This is a direct 1-to-1 translation from AST Unified to AST Imperative
  
  pass ?
    remove : E_Call ( module = "Map" | "BigMap" )
    add    : AST_I.E_Map | AST_I.E_BigMap

    In PascaLIGO :
    > const moves : register =
    >   map [
    >     ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    >     ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
    gives a CST.E_Map, unified into AST_U.E_Map,
    then compiled to AST_I.E_Map with above 'map_to_map' nanopass

    In CameLIGO however,
    > let moves : register =
    >   Map.literal [
    >     (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    >     (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
    Is converted into a E_Call ( E_ModA (name = "Map") ... )
    This is converted somewhere in ktree_abstraction or later into a AST_I.E_Map,
    and we should do the same here.
  
  E_Attr
  =============================================================================
  pass 'check_no_attr'
    remove : E_Attr
    add : nothing | exception

    Attributes in PascaLigo are accumulated during recursive calls to
    [compile_expression], and are checked during evaluation of E_Fun.
    c.f. [check_no_attributes] in [04-tree_abstraction/pascaligo/compiler.ml]
    
    The goal of this nanopass is to accumulate those attributes
    and perform the same check, and then remove the attributes.

  E_CallJsligo
  =============================================================================
  pass 'e_call_jsligo'
    remove : E_CallJsligo
    add    : E_Call | ...

    CST.Jsligo.E_Call represent classic function calls but also some special calls.
    The classic calls will be converted to ASTU.E_Call,
    the special calls will be converted into the correct ASTU node,
    according to what's done in tree-abstraction.

    Special calls are :

    [.] Special calls to 'list'
      | ECall {value=(EVar {value = "list"; _}, Multiple {value = {inside = (EArray {value = {inside = Some (Expr_entry e, [(_, Rest_entry {value = {expr; _}; _})]); _}; _}, []); _}; _}); region } ->
      | ECall {value=(EVar {value = "list"; _}, Multiple {value = {inside = (EArray {value = {inside;lbracket=_;rbracket=_};region=_}, []); _}; _}); region } -> (
    [.] Pattern matching
      | ECall {value=(EVar {value = "match"; _}, Multiple {value = {inside = (input, [(_, EObject {value = {inside = fields; _}; region=finish})]); _}; _}); region=start} ->
      | ECall {value=(EVar {value = "match"; _}, Multiple {value = {inside = (input, [(_, ECall {value = EVar {value="list"; _}, Multiple { value = {inside = (CST.EArray {value = {inside; _}; _}, _); _} ;_} ;_})]); _}; _}); region} ->
    [.] ?
      | ECall {value=(EVar var,args);region} ->
    [X] pseudo-module call
      | ECall ({value=(EModA {value={module_name;field=_;selector=_};region=_} as value,args);region} as call) when List.mem ~equal:String.(=) built_ins module_name.value -> (

      Just convert it into ASTU.E_Call, a later nanopass will convert
      pseudo-module-calls from all syntaxes into appropriate constants.
      

  E_Constr
  =============================================================================
  pass 'abstract_e_constr
    remove : AST_U.E_Constr
    add    : AST_I.E_Constructor
    needs  :
      - tuple_singletons  (removing singleton tuples in ctor arguments)

    0 arg   : E_Constr (name, args = None)                     |-> E_Constructor name E_Unit
    1 args  : E_Constr (name, args = Some e) when e != E_Tuple |-> E_Constructor name e
    2+ args : E_Constr (name, args = Some e) when e == E_Tuple |-> E_Constructor name e

    Note : E_Constr with a E_Tuple with 1 argument are impossible
    because tuple singletons have been removed in [tuple_singleton] nanopass.


  E_ArrayJsligo
  =============================================================================
  pass 'e_array_jsligo
    remove : E_ArrayJsligo
    add    : E_Array

    Almost direct 1-to-1 mapping, except that we raise an exeption on the rest syntax

    E_ArrayJsligo (Expr_entry e1, Expr_entry e2) |-> E_Array [e1, e2]
    E_ArrayJsligo (Expr_entry e1, Rest_entry e2) |-> raise exception

  E_Array
  =============================================================================
  pass 'abstract_array'
    remove : AST_U.E_Array
    add    : AST_I.E_Tuple

    AST Unified's E_Tuple and E_Array are both converted into AST Imperative's E_Tuple.
    The difference is that ASTU.E_Tuple only contain several elements, indeed :
    - E_Tuple take a expr option as arguments, so it cannot contain 0 element.
    - The 'tuple_singleton' nanopass ensures E_Tuple never contains 1 element only.

    Jsligo's arrays, however, can contain several, one, or even 0 elements.
    Jsligo [1, 2, 3] |-> AST_I.E_Tuple [1, 2, 3]
    Jsligo [1]       |-> AST_I.E_Tuple [1]
    Jsligo []        |-> AST_I.E_Tuple []

    If we convert CST.Jsligo.E_Tuple into ASTU.E_Tuple, the 0-uple and 1-uple
    will disappear because of above reasons.

  EObject
  =============================================================================
  pass 'object_jsligo'
    remove : EObject
    add : E_Record | E_Update | exception

    Handle the two cases in jsligo tree-abstraction :
    [ ] | EObject {value = {inside = (Property_rest {value = {expr; _}; _}, rest); _}; _} ->
    [ ] | EObject obj ->
  
    In the first case, object is transformed into nested record accesses
    In the second, it's transformed into a record
  
  EProjJsligo
  =============================================================================
  pass 'proj_jsligo
    remove : E_ProjJsligo
    add    : E_Proj

  It's the same story as E_ModA |-> E_ModPath, going from nested to flat.
  Transform E_ProjJsligo ( E_ProjJsligo (x, y), z) |-> E_Proj (x, [y, z])

  Also, [selection_jsligo]'s Component holds a [expr]
  While [selection]'s Componenent holds a [Z.t]
  We should convert expr |-> Z.t or exception un case of unexpected expression

  E_FunJsligo
  =============================================================================
  pass 'fun_jsligo
    remove : E_FunJsligo
    add    : E_lambda
  
    The work on functions is a bit unclear for now, whatever the syntax,
    but maybe we can merge part of this work between the various syntaxes.
    In which case we should start from a syntax-agnostic E_Fun node.
  
  E_AnnotJsligo
  =============================================================================
  pass 'e_annot_jsligo'
    remove : E_AnnotJsligo
    add    : E_Annot

    E_AnnotJsligo and E_Annot are the same, but with Jsligo, unlike other syntaxes
    certain special sort of E_Annot are converted to other nodes.
    They correspond to the following cases in the Jsligo abstractor :
    | EAnnot {value = (EArith(Int i), _, TVar {value = "nat"; _}); region=_ } ->
    | EAnnot {value = (EArith(Int i), _, TVar {value = "tez"; _}); region=_ } ->
    | EAnnot {value = (EArith(Int i), _, TVar {value = "mutez"; _}); region=_ } ->
    | EAnnot {value = (ECodeInj {value = {language; code};_ }, kwd_as, type_expr); region} ->

    E_AnnotJsligo (special ones) |-> AST.E_some_other_node (see above)
    E_AnnotJsligo (normal ones)  |-> AST.E_Annot
  
  E_AssignJsligo
  =============================================================================
  pass 'e_assign_jsligo'
    remove : E_AssignJsligo
    add    : E_Assign | E_Sequence | exception

    Handle the 4 cases in the jsligo abstractor :
    | EAssign (EVar {value=_; region=_} as e1, op, (EAssign     (EVar _ as ev, _, _) as e2)) ->
    | EAssign (EVar {value; region} as e1, op, e2) ->
    | EAssign (EProj {value = {expr = EVar {value = evar_value; _}; selection = Component {value = {inside = EArith (Int _); _}; _} as selection}; region=_}, ({value = Eq; _} as op), e2) ->
    | EAssign _ as e ->
  
  
  D_Type
  =============================================================================
  pass 'd_type'
    remove : D_TypeDecl
    add    : D_Type

    The goal is to remove the type parameters in the D_TypeDecl node
    and inject them as T_Abstraction.

    D_TypeDecl
      name : my_type_name
      parameters : [ alpha ]
      type_expression : T_xxx
    |->
    D_Type
      binder : my_type_name
      type_expresion :
        T_abstraction( alpha, T_xxx )

  D_Let
  =============================================================================
  pass 'd_let_tuple'
    remove : D_Let ( binder = CST.PTuple, ... )
    add    : D_Let

    The let declarations with a tuple as lhs :
      let x, y = (42, 24)
    Are actually abstracted into several let declarations, one per tuple element :
      let x = (42, 24).0  // (access 1st element of the tuple)
      let y = (42, 24).1  // (access 2nd element of the tuple)
  
  pass 'd_let_record'
    remove : D_Let (binder = CST.PRecord, ...)
    add    : D_Let

    Similarly to nanopass d_let_tuple, this nanopass split let declarations
    with records into several declarations, one per record element.
    For example :
      let {x=z, y} = my_record
    After unpunning, it becomes :
      let {x=z, y=y} = my_record
    With this nanopass d_let_record, it should become :
      let x = my_record.z
      let y = my_record.y
  
  pass 'd_let_lambda'
    remove : D_Let (binders : pattern nseq)
    add    : D_Let' (binder : pattern)

    Replace the list of patterns by E_Lambda :
    let x y z = E_xxx
    |->
    let x = E_lambda(x, E_lambda(y, E_xxx))

  pass 'd_let_type_params'
    remove : D_let (type_params = ...)
    add    : D_let (type_params = [] 
    )
    Add polymorphic binder to ascription
    For each type parameter "alpha",
    Replace rhs type : T_xxx |-> T_For_All(alpha, T_xxx)
  
  pass 'd_let_rec'
    remove : D_Let (is_rec = 1, expr = E_xxx)
    add    : D_Let (is_rec = 0, expr = E_Recursive (E_xxx...))
*)
