module AST = Ast_unified

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
    | (List _) as other -> Sexplib0.Sexp_conv_error.no_matching_variant_found
    "Expected atom but got list"
    other
  
  let sexp_of_t : t -> Sexplib0.Sexp.t = fun t ->
    let s = Format.asprintf "%a" Ty_variable.pp t in
    Sexplib0.Sexp.Atom s
end

module Z_with_sexp = struct
  include Z

  let t_of_sexp : Sexplib0.Sexp.t -> t = function
  | Atom s -> Z.of_string s
  | (List _) as other ->
    Sexplib0.Sexp_conv_error.no_matching_variant_found
    "Expected atom but got list"
    other

  let sexp_of_t : t -> Sexplib0.Sexp.t = fun t ->
    Sexplib0.Sexp.Atom (Z.to_string t)
end

module Loc = struct
  type location = int [@@deriving sexp]
  type 'a t = 'a * location [@@deriving map, sexp]

  (* Just don't print locations for readability *)
  (* TODO : How to add an option to toggle printing of locations on sexp ? *)
  let sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
    fun sexp_of_a t ->
      match sexp_of_t sexp_of_a t with
      | Sexplib0.Sexp.List [sexp_a; _sexp_loc] -> sexp_a
      | _ as other -> other

end

module Mod_variable_with_sexp = struct
  include Mod_variable
  
  let t_of_sexp : Sexp.t -> t = fun t ->
    Sexplib0.Sexp_conv_error.no_matching_variant_found "Unsupported sexp" t

  let sexp_of_t : t -> Sexp.t = fun te ->
    Sexp.List
      [ Sexp.List [ Sexp.Atom "name" ; Sexp.Atom (to_name_exn te) ]
      (* ; Sexp.List [ Sexp.Atom "counter" ; Sexp.Atom (Int.to_string te.counter) ] *)
      ; Sexp.List [ Sexp.Atom "generated" ; Sexp.Atom (Bool.to_string @@ is_generated te) ]
      ]
end

module Literal_value_with_sexp = struct
  include Literal_value

  let t_of_sexp : Sexp.t -> t = fun t ->
    Sexplib0.Sexp_conv_error.no_matching_variant_found "Unsupported sexp" t

  let sexp_of_t : t -> Sexplib0.Sexp.t = fun t ->
    let s = Format.asprintf "%a" Literal_value.pp t in
    Sexp.List [ Sexp.Atom "Literal_value" ; Sexplib0.Sexp.Atom s ]
    
end

module Variable_with_sexp = struct
  include Variable

  let t_of_sexp : Sexp.t -> t = fun t ->
    Sexplib0.Sexp_conv_error.no_matching_variant_found "Unsupported sexp" t

  let sexp_of_t : t -> Sexplib0.Sexp.t = fun t ->
    let s = Format.asprintf "%a" Variable.pp t in
    Sexp.List [ Sexp.Atom "Variable" ; Sexplib0.Sexp.Atom s ]
end



(* ========================================================================= *)
(* ======== Polymorphic AST unified ======================================== *)
(* ========================================================================= *)

type 't type_expr = [
| `T_Var          of (Ty_variable_with_sexp.t) Loc.t
| `T_Prod         of ('t Prod.t) Loc.t
| `T_App          of ((string,'t) Type_app.t) Loc.t
| `T_Fun          of ('t Arrow.t) Loc.t
| `T_Named_fun    of ('t Named_fun.t) Loc.t
| `T_String       of (string) Loc.t
| `T_Int          of (string * Z_with_sexp.t) Loc.t
| `T_ModA         of ((Mod_variable_with_sexp.t, 't) Mod_access.t) Loc.t
| `T_ModPath      of ((Mod_variable_with_sexp.t Simple_utils.List.Ne.t, 't) Mod_access.t) Loc.t
| `T_Arg          of (string) Loc.t
| `T_Sum_raw      of ('t option Non_linear_rows.t) Loc.t
| `T_Arg_sum_raw  of ('t list option Non_linear_rows.t (* "curried" sum-type ["Ctor", arg1, arg2, arg3 ]*)) Loc.t
| `T_Record_raw   of ('t option Non_linear_rows.t) Loc.t
| `T_Disc_union   of ('t Non_linear_disc_rows.t) Loc.t
| `T_Attr         of (Attribute.t * 't) Loc.t
| `T_App_pascaligo of (('t, 't) Type_app.t) Loc.t

| `T_Michelson_or        of (string * 't * string * 't) Loc.t
| `T_Michelson_pair      of (string * 't * string * 't) Loc.t
| `T_Sapling_state       of (string * Z_with_sexp.t) Loc.t
| `T_Sapling_transaction of (string * Z_with_sexp.t) Loc.t

]  [@@deriving map, sexp]


type ('lhs, 'rhs) field =
  | Punned of 'lhs
  | Complete of ('lhs * 'rhs)
  [@@deriving map, sexp]

type 'p list_pattern =
  | Cons of 'p * 'p
  | List of 'p list
  [@@deriving map, sexp]

type ('ty, 'p) pattern = [
| `P_Unit       of ( unit                                   ) Loc.t
| `P_Typed      of ( 'ty * 'p                               ) Loc.t
| `P_Literal    of ( Literal_value_with_sexp.t              ) Loc.t
| `P_Var        of ( Variable_with_sexp.t                   ) Loc.t
| `P_List       of ( 'p list_pattern                        ) Loc.t
| `P_Variant    of ( Label.t * 'p option                    ) Loc.t
| `P_Tuple      of ( 'p list                                ) Loc.t
| `P_Pun_record of ( (Label.t, 'p) field list               ) Loc.t
| `P_Rest       of ( Label.t                                ) Loc.t
| `P_Attr       of ( Attribute.t * 'p                       ) Loc.t
| `P_Mod_access of ( (Mod_variable_with_sexp.t nseq, 'p) Mod_access.t ) Loc.t
] [@@deriving map, sexp]

(* ========================================================================= *)
(* ======== Fixpoints and fold ============================================= *)
(* ========================================================================= *)

type fix_type_expr  = fix_type_expr                type_expr 
and  fix_pattern    = (fix_type_expr, fix_pattern) pattern    
[@@deriving sexp]

let rec fold_type_expr
  (f:'t type_expr -> 't)
  (t:fix_type_expr) : 't =
  f (map_type_expr (fold_type_expr f) t)

let rec fold_pattern
  (ft : 't      type_expr -> 't)
  (fp : ('t,'p) pattern   -> 'p)
  (p  : fix_pattern) : 'p =
    let fold_p : fix_pattern   -> 'p = fold_pattern   ft fp in
    let fold_t : fix_type_expr -> 't = fold_type_expr ft    in
    fp (map_pattern fold_t fold_p p)


(* ========================================================================= *)
(* ======== Small passes and helpers ======================================= *)
(* ========================================================================= *)

let default_compile : Small_passes.syntax -> fix_type_expr -> fix_type_expr =
  fun _syntax te -> te

let default_decompile = default_compile

let default_check_reduction : fix_type_expr -> bool = fun _ -> true

(* Helper used to factor out the common part of all passes' compile functions *)
let wrap_compile (core_compile : fix_type_expr -> fix_type_expr)
  : Small_passes.syntax -> fix_type_expr -> fix_type_expr =
  fun _syntax te -> fold_type_expr core_compile te

let make_pass
  ~(name : string)
  ?(compile = default_compile)
  ?(decompile = default_decompile)
  ?(check_reductions = default_check_reduction)
  (_ : unit)
  : fix_type_expr Small_passes.pass =
  {name; compile; decompile; check_reductions}

let pass_t_arg : fix_type_expr Small_passes.pass =
  let name = "pass_remove_t_arg" in
  let core_compile : fix_type_expr -> fix_type_expr = function
  | `T_Arg (s, loc) -> `T_Var (Ty_variable.of_input_var s, loc)
  | _ as common -> common
  in
  let compile = wrap_compile core_compile in
  let decompile = default_decompile in
  let check_reductions = default_check_reduction in
  {name; compile; decompile; check_reductions}

let pass_t_named_fun : fix_type_expr Small_passes.pass =
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
  let compile = wrap_compile core_compile in
  let decompile = default_decompile in
  let check_reductions = default_check_reduction in
  {name; compile; decompile; check_reductions}

let pass_t_app_pascaligo =
  let name = "pass_t_app_pascaligo" in
  let compile = wrap_compile @@ function
  | `T_App_pascaligo ({constr; type_args}, loc) -> (
    match constr with
    | `T_Var (tv, _loc2) ->
      let constr = Ty_variable_with_sexp.to_name_exn tv in
      `T_App ({constr; type_args}, loc)
    | _ -> failwith "field 'constr' of T_App_pascaligo should be a T_Var"
    )
  | _ as other -> other
  in
  make_pass ~name ~compile ()

let pass_t_app_michelson_types =
  let name = "pass_t_app_michelson_types" in
  let compile = wrap_compile @@ function
  | `T_App ({constr; type_args}, loc) as t -> (
    match constr with
    | "michelson_or" -> (
      match List.Ne.to_list type_args with
      | [ te_left ; `T_String (s_left, _loc) ; te_right ; `T_String (s_right, _loc2)] ->
          `T_Michelson_or ((s_left, te_left, s_right, te_right), loc)
      | _ -> failwith "Wrong number of arguments for michelson_or"
      )
    | "michelson_pair" -> (
      match List.Ne.to_list type_args with
      | [ te_left ; `T_String (s_left, _loc) ; te_right ; `T_String (s_right, _loc2)] ->
          `T_Michelson_pair ((s_left, te_left, s_right, te_right), loc)
      | _ -> failwith "Wrong number of arguments for michelson_pair"
      )
    | "sapling_state" -> (
      match List.Ne.to_list type_args with
      | [`T_Int ((annot, z), loc)] -> `T_Sapling_state ((annot, z), loc)
      | [_] -> failwith "Expect a T_int as argument to the T_Sapling_state"
      | _ -> failwith "Wrong number of arguments for sapling_state"
      )
    | "sapling_transaction" -> (
      match List.Ne.to_list type_args with
      | [`T_Int ((annot, z), loc)] -> `T_Sapling_transaction ((annot, z), loc)
      | [_] -> failwith "Expect a T_int as argument to the T_Sapling_transaction"
      | _ -> failwith "Wrong number of arguments for sapling_transaction"
      )
    | _ -> t
    )
  | _ as common -> common
  in
  make_pass ~name ~compile ()

let pass_t_string_and_int_unsupported =
  let name = "pass_t_string_and_int_unsupported" in
  let compile = wrap_compile @@ function
  | `T_Int _ -> failwith "Invalid type T_Int at this stage"
  | `T_String _ -> failwith "Invalid type T_String at this stage"
  | _ as other -> other
  in
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