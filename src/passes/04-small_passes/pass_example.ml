module AST = Ast_unified
module Directive = Ast_unified.Directive

type 'a expr = AST.expr
type fix_expr (*dummy ... after we have AST.types == polyvariants *)
type 'a nseq = 'a Simple_utils.List.Ne.t [@@deriving map, sexp]

(* type ty_variable = [%import: Ty_variable.t] [@@deriving sexp] *)

(* module Tyvar = Ty_variable [@@deriving sexp] *)

open AST

(* ========================================================================= *)
(* ======== Loc and modules with sexp ====================================== *)
(* ========================================================================= *)

module Ty_variable_with_sexp = struct
  include Ty_variable

  let t_of_sexp : Sexplib0.Sexp.t -> t = function
    | Atom s -> Ty_variable.of_input_var s
    | List _ as other ->
      Sexplib0.Sexp_conv_error.no_matching_variant_found
        "Expected atom but got list"
        other


  let sexp_of_t : t -> Sexplib0.Sexp.t =
   fun t ->
    let s = Format.asprintf "%a" Ty_variable.pp t in
    Sexplib0.Sexp.Atom s
end

module Z_with_sexp = struct
  include Z

  let t_of_sexp : Sexplib0.Sexp.t -> t = function
    | Atom s -> Z.of_string s
    | List _ as other ->
      Sexplib0.Sexp_conv_error.no_matching_variant_found
        "Expected atom but got list"
        other


  let sexp_of_t : t -> Sexplib0.Sexp.t = fun t -> Sexplib0.Sexp.Atom (Z.to_string t)
end

module Loc = struct
  type location = int [@@deriving sexp]
  type 'a t = 'a * location [@@deriving map, sexp]

  (* Just don't print locations for readability *)
  (* TODO : How to add an option to toggle printing of locations on sexp ? *)
  let sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
   fun sexp_of_a t ->
    match sexp_of_t sexp_of_a t with
    | Sexplib0.Sexp.List [ sexp_a; _sexp_loc ] -> sexp_a
    | _ as other -> other
end

module Mod_variable_with_sexp = struct
  include Mod_variable

  let t_of_sexp : Sexp.t -> t =
   fun t -> Sexplib0.Sexp_conv_error.no_matching_variant_found "Unsupported sexp" t


  let sexp_of_t : t -> Sexp.t =
   fun te ->
    Sexp.List
      [ Sexp.List [ Sexp.Atom "name"; Sexp.Atom (to_name_exn te) ]
        (* ; Sexp.List [ Sexp.Atom "counter" ; Sexp.Atom (Int.to_string te.counter) ] *)
      ; Sexp.List [ Sexp.Atom "generated"; Sexp.Atom (Bool.to_string @@ is_generated te) ]
      ]
end

module Literal_value_with_sexp = struct
  include Literal_value

  let t_of_sexp : Sexp.t -> t =
   fun t -> Sexplib0.Sexp_conv_error.no_matching_variant_found "Unsupported sexp" t


  let sexp_of_t : t -> Sexplib0.Sexp.t =
   fun t ->
    let s = Format.asprintf "%a" Literal_value.pp t in
    Sexp.List [ Sexp.Atom "Literal_value"; Sexplib0.Sexp.Atom s ]
end

module Variable_with_sexp = struct
  include Variable

  let t_of_sexp : Sexp.t -> t =
   fun t -> Sexplib0.Sexp_conv_error.no_matching_variant_found "Unsupported sexp" t


  let sexp_of_t : t -> Sexplib0.Sexp.t =
   fun t ->
    let s = Format.asprintf "%a" Variable.pp t in
    Sexp.List [ Sexp.Atom "Variable"; Sexplib0.Sexp.Atom s ]
end

module Constructor_with_sexp = struct
  include Constructor 

  (* let t_of_sexp : Sexp.t -> 'a t =
   fun t -> Sexplib0.Sexp_conv_error.no_matching_variant_found "Unsupported sexp" t

  let sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
   fun sexp_of_elt t ->
    Sexp.List
      [ Sexp.Atom "Constructor"
      ; Sexp.List [ Sexp.Atom "constructor" ; Label.sexp_of_t t.constructor ]
      ; Sexp.List [ Sexp.Atom "element" ; sexp_of_elt t.element  ]
      ] *)

    let t_of_sexp = fun _ -> failwith "cannot enable sexp"
    let sexp_of_t = fun _ -> failwith "cannot enable sexp"
end

module Raw_code_with_sexp = struct
  include Raw_code
  let t_of_sexp = fun _ -> failwith "cannot enable sexp"
  let sexp_of_t = fun _ -> failwith "cannot enable sexp"
end


(* ========================================================================= *)
(* ======== Polymorphic AST unified ======================================== *)
(* ========================================================================= *)

type 't type_expr =
  [ `T_Var of Ty_variable_with_sexp.t Loc.t
  | `T_Prod of 't Prod.t Loc.t
  | `T_App of (string, 't) Type_app.t Loc.t
  | `T_Fun of 't Arrow.t Loc.t
  | `T_Named_fun of 't Named_fun.t Loc.t
  | `T_String of string Loc.t
  | `T_Int of (string * Z_with_sexp.t) Loc.t
  | `T_ModA of (Mod_variable_with_sexp.t, 't) Mod_access.t Loc.t
  | `T_ModPath of (Mod_variable_with_sexp.t Simple_utils.List.Ne.t, 't) Mod_access.t Loc.t
  | `T_Arg of string Loc.t
  | `T_Sum_raw of 't option Non_linear_rows.t Loc.t
  | `T_Arg_sum_raw of
    't list option Non_linear_rows.t (* "curried" sum-type ["Ctor", arg1, arg2, arg3 ]*)
    Loc.t
  | `T_Record_raw of 't option Non_linear_rows.t Loc.t
  | `T_Disc_union of 't Non_linear_disc_rows.t Loc.t
  | `T_Attr of (Attribute.t * 't) Loc.t
  | `T_App_pascaligo of ('t, 't) Type_app.t Loc.t
  | `T_Michelson_or of (string * 't * string * 't) Loc.t
  | `T_Michelson_pair of (string * 't * string * 't) Loc.t
  | `T_Sapling_state of (string * Z_with_sexp.t) Loc.t
  | `T_Sapling_transaction of (string * Z_with_sexp.t) Loc.t
  ]
[@@deriving map, sexp]

type ('lhs, 'rhs) field =
  | Punned of 'lhs
  | Complete of ('lhs * 'rhs)
[@@deriving map, sexp]

type 'p list_pattern =
  | Cons of 'p * 'p
  | List of 'p list
[@@deriving map, sexp]

type ('ty, 'p) pattern =
  [ `P_Unit of unit Loc.t
  | `P_Typed of ('ty * 'p) Loc.t
  | `P_Literal of Literal_value_with_sexp.t Loc.t
  | `P_Var of Variable_with_sexp.t Loc.t
  | `P_List of 'p list_pattern Loc.t
  | `P_Variant of (Label.t * 'p option) Loc.t
  | `P_Tuple of 'p list Loc.t
  | `P_Pun_record of (Label.t, 'p) field list Loc.t
  | `P_Rest of Label.t Loc.t
  | `P_Attr of (Attribute.t * 'p) Loc.t
  | `P_Mod_access of (Mod_variable_with_sexp.t nseq, 'p) Mod_access.t Loc.t
  ]
[@@deriving map, sexp]

type ('i, 'e, 'p, 's) instruction =
  [ `I_struct_assign   of ( 'e Struct_assign.t                        ) Loc.t
  | `I_Call            of ( 'e Instruction_call.t                     ) Loc.t
  | `I_Case            of ( ('e, 'p, ('i, 's) Test_clause.t) Case.t   ) Loc.t
  | `I_Cond            of ( ('e, ('i, 's) Test_clause.t) Cond.t       ) Loc.t
  | `I_For             of ( ('e, 's) For_int.t                        ) Loc.t
  | `I_ForIn           of ( ('e, 's) For_collection.t                 ) Loc.t
  | `I_ForOf           of ( ('e, 's) For_of.t                         ) Loc.t
  | `I_Patch           of ( 'e Patch.t                                ) Loc.t
  | `I_Remove          of ( 'e Removal.t                              ) Loc.t
  | `I_Skip            of ( unit                                      ) Loc.t
  | `I_While           of ( ('e, 's) While.t                          ) Loc.t
  | `I_Block           of ( 's Simple_utils.List.Ne.t                 ) Loc.t
  | `I_Expr            of ( 'e                                        ) Loc.t
  | `I_Return          of ( 'e option                                 ) Loc.t
  | `I_Switch          of ( ('e, 's) Switch.t                         ) Loc.t
  | `I_break           of ( unit                                      ) Loc.t
  ]
[@@deriving map, sexp]


type ('s, 'i, 'd) statement =
  [ `S_Attr            of ( (Attribute.t * 's)                        ) Loc.t
  | `S_Instr           of ( 'i                                        ) Loc.t
  | `S_Decl            of ( 'd                                        ) Loc.t
  ]
[@@deriving map, sexp]

type ('d, 'e, 'te, 'p, 'm) declaration =
  [ `D_Directive       of ( Directive.t                               ) Loc.t  
  | `D_Attr            of ( (Attribute.t * 'd)                        ) Loc.t  
  | `D_Import          of ( Import.t                                  ) Loc.t  
  | `D_Export          of ( 'd                                        ) Loc.t  
  | `D_Let             of ( ('e, 'p nseq, 'te) Let_decl.t             ) Loc.t  
  | `D_Var             of ( ('p, 'e, 'te) Simple_decl.t               ) Loc.t  
  | `D_Multi_var       of ( ('p, 'e, 'te) Simple_decl.t nseq          ) Loc.t  
  | `D_Const           of ( ('p, 'e, 'te) Simple_decl.t               ) Loc.t  
  | `D_Multi_const     of ( ('p, 'e, 'te) Simple_decl.t nseq          ) Loc.t  
  | `D_Fun             of ( ('te, 'e, ('p, 'te) Param.t) Fun_decl.t   ) Loc.t  
  | `D_Type            of ( 'te Type_decl.t                           ) Loc.t  
  | `D_Module          of ( 'm Mod_decl.t                             ) Loc.t  
  ]
[@@deriving map, sexp]

type ('s, 'd) module_expression =
  [ `M_Body_statements of ( 's nseq                                   ) Loc.t
  | `M_Body            of ( 'd nseq                                   ) Loc.t
  | `M_Path            of ( Ligo_prim.Module_var.t nseq               ) Loc.t
  | `M_Var             of ( Ligo_prim.Module_var.t                    ) Loc.t
  ]
[@@deriving map, sexp]

type ('e, 'te, 'p, 's, 'm) expression =
  [ `E_Attr            of ( (Attribute.t * 'e)                        ) Loc.t  
  | `E_Literal         of ( Literal_value.t                           ) Loc.t  
  (* | `E_Binary_op       of ( 'e Operators.binary_op                    ) Loc.t *)
  (* | `E_Unary_op        of ( 'e Operators.unary_op                     ) Loc.t *)
  | `E_variable        of ( Variable.t                                ) Loc.t  
  | `E_RevApp          of ( 'e Rev_app.t                              ) Loc.t  
  | `E_Tuple           of ( 'e nseq                                   ) Loc.t  
  | `E_Record_pun      of ( (Variable.t, 'e) Field.t list             ) Loc.t  
  | `E_Array           of ( 'e Array_repr.t                           ) Loc.t  
  | `E_Object          of ( 'e Object_.t                              ) Loc.t  
  | `E_List            of ( 'e list                                   ) Loc.t  
  | `E_Proj            of ( 'e Projection.t                           ) Loc.t  
  | `E_ModA            of ( (string, 'e) Mod_access.t                 ) Loc.t  
  | `E_ModPath         of ( (string nseq, 'e) Mod_access.t            ) Loc.t
  | `E_Update          of ( 'e Update.t                               ) Loc.t
  | `E_Poly_fun        of ( ('e, 'te, 'p) Poly_fun.t                  ) Loc.t  
  | `E_Block_fun       of ( ('e, 'te, 's) Block_fun.t                 ) Loc.t
  | `E_Constr          of ( 'e option Constructor_with_sexp.t         ) Loc.t  
  | `E_App             of ( ('e * 'e nseq option)                     ) Loc.t  
  | `E_Call            of ( 'e * 'e nseq                              ) Loc.t  
  | `E_Case            of ( ('e, 'p, 'e) Case.t                       ) Loc.t  
  | `E_Annot           of ( ('e * 'te)                                ) Loc.t  
  | `E_Cond            of ( ('e, 'e) Cond.t                           ) Loc.t  
  | `E_Set             of ( 'e list                                   ) Loc.t  
  | `E_MapLookup       of ( 'e Map_lookup.t                           ) Loc.t
  | `E_Map             of ( ('e * 'e) list                            ) Loc.t
  | `E_BigMap          of ( ('e * 'e) list                            ) Loc.t
  | `E_Let_in          of ( ('p, 'e, 'te) Let_binding.t               ) Loc.t  
  | `E_TypeIn          of ( ('e, 'te) Type_in.t                       ) Loc.t  
  | `E_ModIn           of ( ('e, 'm) Mod_in.t                         ) Loc.t  
  | `E_RawCode         of ( 'e Raw_code_with_sexp.t                   ) Loc.t
  | `E_Sequence        of ( ('e * 'e)                                 ) Loc.t
  | `E_Block_with      of ( ('e, 's) Block_with.t                     ) Loc.t
  | `E_AssignJsligo    of ( 'e Assign_jsligo.t                        ) Loc.t  
  ]
[@@deriving map, sexp]

type ('prog_entry, 'd, 'i) program_entry =
  [ `P_Attr                   of ( Attribute.t * 'prog_entry          ) Loc.t
  | `P_Declaration            of ( 'd                                 ) Loc.t
  | `P_Top_level_instruction  of ( 'i                                 ) Loc.t
  | `P_Directive              of ( Directive.t                        ) Loc.t
  ]
[@@deriving map, sexp]


(* ========================================================================= *)
(* ======== Fixpoints and fold ============================================= *)
(* ========================================================================= *)

(* The use of shortcut name (e.g. 'fix_p') here is just to break long lines,
   the short names are aliased to their full name right after below *)
type fix_t  = fix_t                                    type_expr
and  fix_p  = (fix_t , fix_p)                          pattern 
and  fix_i  = (fix_i, fix_e, fix_p, fix_s)             instruction
and  fix_s  = (fix_s, fix_i, fix_d)                    statement
and  fix_d  = (fix_d, fix_e, fix_t , fix_p, fix_m)     declaration
and  fix_m  = (fix_s, fix_d)                           module_expression
and  fix_e  = (fix_e, fix_t , fix_p, fix_s, fix_m)     expression
and  fix_pe = (fix_pe, fix_d, fix_i)                   program_entry
[@@deriving sexp]

type fix_type_expr         = fix_t  [@@deriving sexp]
type fix_pattern           = fix_p  [@@deriving sexp]
type fix_instruction       = fix_i  [@@deriving sexp]
type fix_statement         = fix_s  [@@deriving sexp]
type fix_declaration       = fix_d  [@@deriving sexp]
type fix_module_expression = fix_m  [@@deriving sexp]
type fix_expression        = fix_e  [@@deriving sexp]
type fix_program_entry     = fix_pe [@@deriving sexp]

(* These are all the functions you must specify when implementing a pass.
   If you want a pass that modifies only patterns (for example),
   you should only touch 'fp' and leave the others to the default value,
   i.e. the identity function *)
type ('t, 'p, 'i, 's, 'd, 'm, 'e, 'pe) folders =
  { ft : 't                    type_expr          -> 't
  ; fp : ('t, 'p)              pattern            -> 'p
  ; fi : ('i, 'e, 'p, 's)      instruction        -> 'i
  ; fs : ('s, 'i, 'd)          statement          -> 's
  ; fd : ('d, 'e, 't, 'p, 'm)  declaration        -> 'd
  ; fm : ('s, 'd)              module_expression  -> 'm
  ; fe : ('e, 't, 'p, 's, 'm)  expression         -> 'e
  ; fpe : ('pe, 'd, 'i)        program_entry      -> 'pe
  }

let folders_default =
  { ft  = Fn.id
  ; fp  = Fn.id
  ; fi  = Fn.id
  ; fs  = Fn.id
  ; fd  = Fn.id
  ; fm  = Fn.id
  ; fe  = Fn.id
  ; fpe = Fn.id
  }

(*
  Below fold functions are mutually recursive
  necause their types are mutually recursive.
  For example,
  expr can contain type_expr (so fold_e needs fold_te),
  and type_expr can contain expr (so fold_te needs fold_e).
*)
let rec fold_t
  (folders : ('t, 'p, 'i, 's, 'd, 'm, 'e, 'pe) folders)
  (t : fix_type_expr)
  : 't =
  folders.ft (map_type_expr (fold_t folders) t)

and fold_p
  (folders : ('t, 'p, 'i, 's, 'd, 'm, 'e, 'pe) folders)
  (p : fix_pattern)
  : 'p
  =
  let fold_p = fold_p folders in
  let fold_t = fold_t folders in
  folders.fp (map_pattern fold_t fold_p p)

and fold_i
  (folders : ('t, 'p, 'i, 's, 'd, 'm, 'e, 'pe) folders)
  (i : fix_instruction)
  : 'i
  =
  let fold_p = fold_p folders in
  let fold_i = fold_i folders in
  let fold_s = fold_s folders in
  let fold_e = fold_e folders in
  folders.fi (map_instruction fold_i fold_e fold_p fold_s i)

and fold_s
  (folders : ('t, 'p, 'i, 's, 'd, 'm, 'e, 'pe) folders)
  (s : fix_statement)
  : 's
  =
  let fold_i = fold_i folders in
  let fold_s = fold_s folders in
  let fold_d = fold_d folders in
  folders.fs (map_statement fold_s fold_i fold_d s)

and fold_d
  (folders : ('t, 'p, 'i, 's, 'd, 'm, 'e, 'pe) folders)
  (d : fix_declaration)
  : 'd
  =
  let fold_t = fold_t folders in
  let fold_p = fold_p folders in
  let fold_d = fold_d folders in
  let fold_m = fold_m folders in
  let fold_e = fold_e folders in
  folders.fd (map_declaration fold_d fold_e fold_t fold_p fold_m d)

and fold_m
  (folders : ('t, 'p, 'i, 's, 'd, 'm, 'e, 'pe) folders)
  (m : fix_module_expression)
  : 'm
  =
  let fold_s = fold_s folders in
  let fold_d = fold_d folders in
  folders.fm (map_module_expression fold_s fold_d m)

and fold_e
  (folders : ('t, 'p, 'i, 's, 'd, 'm, 'e, 'pe) folders)
  (e : fix_expression)
  : 'e
  =
  let fold_t = fold_t folders in
  let fold_p = fold_p folders in
  let fold_s = fold_s folders in
  let fold_m = fold_m folders in
  let fold_e = fold_e folders in
  folders.fe (map_expression fold_e fold_t fold_p fold_s fold_m e)

and fold_pe
  (folders : ('t, 'p, 'i, 's, 'd, 'm, 'e, 'pe) folders)
  (pe : fix_program_entry)
  : 'pe
  =
  let fold_i = fold_i folders in
  let fold_d = fold_d folders in
  let fold_pe = fold_pe folders in
  folders.fpe (map_program_entry fold_pe fold_d fold_i pe)


let fold_type_expr         = fold_t
let fold_pattern           = fold_p
let fold_instruction       = fold_i
let fold_statement         = fold_s
let fold_declaration       = fold_d
let fold_module_expression = fold_m
let fold_expression        = fold_e
let fold_program_entry     = fold_pe

 

(* ========================================================================= *)
(* ======== Small passes and helpers ======================================= *)
(* ========================================================================= *)

let default_compile : Passes.syntax -> 'a -> 'a = fun _syntax a -> a
let default_decompile = default_compile
let default_check_reduction : 'a -> bool = fun _ -> true

(* Helper used to factor out the common part of all passes' compile functions *)
let wrap_compile_t (core_compile : fix_type_expr -> fix_type_expr)
    : Passes.syntax -> fix_type_expr -> fix_type_expr
  =
  let folders = {folders_default with ft = core_compile} in
 fun _syntax te -> fold_type_expr folders te


let wrap_compile_p
    (core_compile_t : fix_type_expr -> fix_type_expr)
    (core_compile_p : fix_pattern -> fix_pattern)
    : Passes.syntax -> fix_pattern -> fix_pattern
  =
  let folders = {folders_default with
    ft = core_compile_t;
    fp = core_compile_p }
  in
  fun _syntax p -> fold_pattern folders p


let make_pass
    ~(name : string)
    ?(compile = default_compile)
    ?(decompile = default_decompile)
    ?(check_reductions = default_check_reduction)
    (_ : unit)
    : 'a Passes.pass
  =
  { name; compile; decompile; check_reductions }


let pass_t_arg : fix_type_expr Passes.pass =
  let name = "pass_remove_t_arg" in
  let core_compile : fix_type_expr -> fix_type_expr = function
    | `T_Arg (s, loc) -> `T_Var (Ty_variable.of_input_var s, loc)
    | _ as common -> common
  in
  let compile = wrap_compile_t core_compile in
  let decompile = default_decompile in
  let check_reductions = default_check_reduction in
  { name; compile; decompile; check_reductions }


let pass_t_named_fun : fix_type_expr Passes.pass =
  let name = "pass_remove_t_named_fun" in
  let core_compile : fix_type_expr -> fix_type_expr = function
    | `T_Named_fun ((args, f), loc) ->
      let remove_name (t : fix_type_expr Named_fun.fun_type_arg) = t.type_expr in
      let args : fix_type_expr list = List.map ~f:remove_name args in
      (* We have `f`, we have `args = [a1; a2; an]`
       we want T_fun f (T_fun a1 (T_fun a2 an))
       hence the below fold over [a2; a1; f] starting with `an` *)
      let (an, l) : fix_type_expr nseq = List.Ne.rev (f, args) in
      let res = List.fold ~init:an ~f:(fun acc t -> `T_Fun ((t, acc), loc)) l in
      res
    | _ as common -> common
  in
  let compile = wrap_compile_t core_compile in
  let decompile = default_decompile in
  let check_reductions = default_check_reduction in
  { name; compile; decompile; check_reductions }


let pass_t_app_pascaligo =
  let name = "pass_t_app_pascaligo" in
  let compile =
    wrap_compile_t
    @@ function
    | `T_App_pascaligo ({ constr; type_args }, loc) ->
      (match constr with
      | `T_Var (tv, _loc2) ->
        let constr = Ty_variable_with_sexp.to_name_exn tv in
        `T_App ({ constr; type_args }, loc)
      | _ -> failwith "field 'constr' of T_App_pascaligo should be a T_Var")
    | _ as other -> other
  in
  make_pass ~name ~compile ()


let pass_t_app_michelson_types =
  let name = "pass_t_app_michelson_types" in
  let compile =
    wrap_compile_t
    @@ function
    | `T_App ({ constr; type_args }, loc) as t ->
      (match constr with
      | "michelson_or" ->
        (match List.Ne.to_list type_args with
        | [ te_left; `T_String (s_left, _loc); te_right; `T_String (s_right, _loc2) ] ->
          `T_Michelson_or ((s_left, te_left, s_right, te_right), loc)
        | _ -> failwith "Wrong number of arguments for michelson_or")
      | "michelson_pair" ->
        (match List.Ne.to_list type_args with
        | [ te_left; `T_String (s_left, _loc); te_right; `T_String (s_right, _loc2) ] ->
          `T_Michelson_pair ((s_left, te_left, s_right, te_right), loc)
        | _ -> failwith "Wrong number of arguments for michelson_pair")
      | "sapling_state" ->
        (match List.Ne.to_list type_args with
        | [ `T_Int ((annot, z), loc) ] -> `T_Sapling_state ((annot, z), loc)
        | [ _ ] -> failwith "Expect a T_int as argument to the T_Sapling_state"
        | _ -> failwith "Wrong number of arguments for sapling_state")
      | "sapling_transaction" ->
        (match List.Ne.to_list type_args with
        | [ `T_Int ((annot, z), loc) ] -> `T_Sapling_transaction ((annot, z), loc)
        | [ _ ] -> failwith "Expect a T_int as argument to the T_Sapling_transaction"
        | _ -> failwith "Wrong number of arguments for sapling_transaction")
      | _ -> t)
    | _ as common -> common
  in
  make_pass ~name ~compile ()


let pass_t_string_and_int_unsupported =
  let name = "pass_t_string_and_int_unsupported" in
  let compile =
    wrap_compile_t
    @@ function
    | `T_Int _ -> failwith "Invalid type T_Int at this stage"
    | `T_String _ -> failwith "Invalid type T_String at this stage"
    | _ as other -> other
  in
  make_pass ~name ~compile ()


(* First dummy passes on patterns *)

let identity : 'a -> 'a = fun x -> x
let default_compile_t = identity

(*
  This example tests what happens when we touch both the patterns and types in a same AST unified instance
  The compile_t and compile_p do random dummy stuffs on type_expression and patterns, independently of each other.
  On an AST with a typed pattern, we should observe the two transformations performed.
*)
let pass_p_typed_toy =
  let name = "pass_p_typed_toy" in
  let compile_t : fix_type_expr -> fix_type_expr = function
    | `T_Arg (s, loc) -> `T_Var (Ty_variable_with_sexp.of_input_var s, loc)
    | _ as other -> other
  in
  let compile_p : fix_pattern -> fix_pattern = function
    | `P_Var (_, loc) -> `P_Var (Variable_with_sexp.of_input_var "renamed_toto", loc)
    | _ as other -> other
  in
  let compile = wrap_compile_p compile_t compile_p in
  make_pass ~name ~compile ()

(* ========================================================================= *)
(* ======== Small passes TODO list form list_passes.md ===================== *)
(* ========================================================================= *)

(*
From [cat list_passes.md | grep "pass 't_"]
[ ]  pass 't_sum'
[ ]  pass 't_prod'
[ ]  pass 't_fun'
[ ]  pass 't_var'
[ ]  pass 't_record'
[O]  pass 't_recordcameligo'
[O]  pass 't_par'
[X]  pass 't_app_pascaligo'
[X]  pass 't_app_michelson_types'
[ ]  pass 't_app'
[X]  pass 't_string_and_int_unsupported' :
[ ]  pass 't_moda'
[ ]  pass 't_modpath'
[ ]  pass 't_attr'
[ ]  pass 't_record_pascaligo'
[ ]  pass 't_object'
[ ]  pass 't_disc' (or 'uncurry_sum_type')
*)
