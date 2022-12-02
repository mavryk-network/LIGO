(* Name shortcuts *)
(* module SP = Small_passes.Passes *)
(* module PE = Small_passes.Pass_example *)
(* module AST = Ast_unified *)

(* Ligo_prim dependencies *)
module Label = Ligo_prim.Label

let () = Format.printf "SEcond Hello world from small_passes_main@."

(* Some little tests *)

(* let titi_1 : PE.fix_type_expr = `T_Arg ("titi", 42)

let titi_2 : PE.fix_type_expr =
  let rhs : PE.fix_type_expr option AST.Types.Non_linear_rows.row_element =
    { associated_type = Some titi_1; attributes = []; decl_pos = 42 }
  in
  let record = [ Label.of_string "lhs_toto", rhs ] in
  `T_Record_raw (record, 42)


(* Main test *)

type te = PE.fix_type_expr
type te_pass = te SP.pass

let passes_list =
  let open PE in
  [ pass_t_arg
  ; pass_t_named_fun
  ; pass_t_app_pascaligo
  ; pass_t_app_michelson_types
  ; pass_t_string_and_int_unsupported
  ]


let ghost_loc : 'a -> 'a PE.Loc.t = fun x -> x, 0
let tvar_of_str = AST.Types.Ty_variable.of_input_var

let my_fun : te =
  let args : te AST.Named_fun.fun_type_args =
    [ { name = "arg_1"; type_expr = `T_Var (ghost_loc @@ tvar_of_str "a1") }
    ; { name = "arg_2"; type_expr = `T_Var (ghost_loc @@ tvar_of_str "a2") }
    ; { name = "arg_3"; type_expr = `T_Var (ghost_loc @@ tvar_of_str "a3") }
    ]
  in
  let f : te = `T_Var (ghost_loc @@ tvar_of_str "f") in
  `T_Named_fun (ghost_loc (args, f))


let my_app_pascaligo : te =
  let loc = 0 in
  let constr = `T_Var (ghost_loc @@ tvar_of_str "f") in
  let type_args : te Simple_utils.List.Ne.t =
    ( `T_Var (ghost_loc @@ tvar_of_str "x")
    , [ `T_Var (ghost_loc @@ tvar_of_str "y"); `T_Var (ghost_loc @@ tvar_of_str "z") ] )
  in
  `T_App_pascaligo ({ constr; type_args }, loc)


let my_app_pascaligo_wrong : te =
  let loc = 0 in
  let constr = my_app_pascaligo in
  let type_args : te Simple_utils.List.Ne.t =
    ( `T_Var (ghost_loc @@ tvar_of_str "a")
    , [ `T_Var (ghost_loc @@ tvar_of_str "b"); `T_Var (ghost_loc @@ tvar_of_str "c") ] )
  in
  `T_App_pascaligo ({ constr; type_args }, loc)


let my_michelson_pair : te =
  let loc = 42 in
  let constr = "michelson_pair" in
  let type_args : te Simple_utils.List.Ne.t =
    ( `T_Var (ghost_loc @@ tvar_of_str "my_first_arg")
    , [ `T_Var (ghost_loc @@ tvar_of_str "my_second_arg") ] )
  in
  `T_App ({ constr; type_args }, loc)


let my_michelson_3uple : te =
  let loc = 42 in
  let constr = "michelson_pair" in
  let type_args : te Simple_utils.List.Ne.t =
    ( `T_Var (ghost_loc @@ tvar_of_str "my_first_arg")
    , [ `T_Var (ghost_loc @@ tvar_of_str "my_second_arg")
      ; `T_Var (ghost_loc @@ tvar_of_str "my_unexpected_3rd_arg")
      ] )
  in
  `T_App ({ constr; type_args }, loc)


let my_michelson_or : te =
  let loc = 42 in
  let constr = "michelson_or" in
  let type_args : te Simple_utils.List.Ne.t =
    ( `T_Var (ghost_loc @@ tvar_of_str "my_first_arg")
    , [ `T_Var (ghost_loc @@ tvar_of_str "my_second_arg") ] )
  in
  `T_App ({ constr; type_args }, loc)


let my_sapling_state : te =
  let loc = 0 in
  let t_int = `T_Int (ghost_loc ("annot", Z.of_int 42)) in
  let type_args = Simple_utils.List.Ne.singleton t_int in
  let constr = "sapling_state" in
  `T_App ({ constr; type_args }, loc)


let my_sapling_state_wrong : te =
  let loc = 0 in
  let not_t_int = `T_Var (ghost_loc @@ tvar_of_str "i_should_be_a_t_int") in
  let type_args = Simple_utils.List.Ne.singleton not_t_int in
  let constr = "sapling_state" in
  `T_App ({ constr; type_args }, loc)


let my_sapling_transaction : te =
  let loc = 0 in
  let t_int = `T_Int (ghost_loc ("annot", Z.of_int 42)) in
  let type_args = Simple_utils.List.Ne.singleton t_int in
  let constr = "sapling_transaction" in
  `T_App ({ constr; type_args }, loc)


let my_sapling_transaction_wrong : te =
  let loc = 0 in
  let not_t_int = `T_Var (ghost_loc @@ tvar_of_str "i_should_be_a_t_int") in
  let type_args = Simple_utils.List.Ne.singleton not_t_int in
  let constr = "sapling_transaction" in
  `T_App ({ constr; type_args }, loc)


let single_t_int : te = `T_Int (ghost_loc ("toto", Z.of_int 42))
let single_t_string : te = `T_String (ghost_loc "i_should_be_in_michelson_type_or_t_disc")

let inputs : (string * te) list =
  [ "simple_arg", `T_Arg (ghost_loc "my_arg")
  ; "simple_var", `T_Var (ghost_loc @@ tvar_of_str "my_var")
  ; "named_fun", my_fun
  ; "my_app_pascaligo", my_app_pascaligo
  ; "my_app_pascaligo_wrong", my_app_pascaligo_wrong
  ; "michelson_pair", my_michelson_pair
  ; "michelson_3uple", my_michelson_3uple
  ; "michelson_or", my_michelson_or
  ; "my_sapling_state", my_sapling_state
  ; "my_sapling_state_wrong", my_sapling_state_wrong
  ; "my_sapling_transaction", my_sapling_transaction
  ; "my_sapling_transaction_wrong", my_sapling_transaction_wrong
  ; "single_t_int", single_t_int
  ; "single_t_string", single_t_string
  ]


let test_input
    (passes : 'a SP.pass list)
    (sexp_of_t : 'a -> Sexp.t)
    ((test_name, input) : string * 'a)
  =
  let ppf = Format.std_formatter in
  let () = Ansi.add_ansi_marking ppf in
  let print_sexp_with_header ppf sexp_of_t header_str t =
    let sexp = sexp_of_t t in
    Format.fprintf ppf "@[<v 2>%s :@,%a@]@." header_str Sexplib0.Sexp.pp_hum sexp
  in
  Format.fprintf ppf "@.@{<bold>@{<underline>Test : %s@}@}@." test_name;
  print_sexp_with_header ppf sexp_of_t "Input" input;
  try
    let output = Small_passes.compile_with_passes ~syntax_todo:() passes [] input in
    print_sexp_with_header ppf sexp_of_t "Output" output
  with
  | e -> Format.fprintf ppf "Exception : %s@." (Exn.to_string e)


let dummy_p = `P_Var (ghost_loc @@ PE.Variable_with_sexp.of_input_var "dummy_p")
let my_typed_p = `P_Typed (ghost_loc (`T_Arg ("my_arg", 42), dummy_p))

let p_inputs : (string * PE.fix_pattern) list =
  [ "dummy_p", dummy_p; "typed_p", my_typed_p ]


let p_passes_list : PE.fix_pattern SP.pass list =
  let open PE in
  [ pass_p_typed_toy ]


let () = List.iter ~f:(test_input passes_list PE.sexp_of_fix_type_expr) inputs
let () = List.iter ~f:(test_input p_passes_list PE.sexp_of_fix_pattern) p_inputs *)



(* sexp test with ast unified *)

(* let my_ty_expr : Ast_unified.ty_expr =
  let loc = Ast_unified.Location.dummy in
  let args : Ast_unified.ty_expr Ast_unified.Named_fun.fun_type_args =
    [ { name = "arg_1"; type_expr = Ast_unified.t_var ~loc (Ast_unified.Ty_variable.of_input_var "a1")}
    ; { name = "arg_2"; type_expr = Ast_unified.t_var ~loc (Ast_unified.Ty_variable.of_input_var "a2")}
    ; { name = "arg_3"; type_expr = Ast_unified.t_var ~loc (Ast_unified.Ty_variable.of_input_var "a3")}
    ]
  in
  let f : Ast_unified.ty_expr = Ast_unified.t_var ~loc (Ast_unified.Ty_variable.of_input_var "f") in
  Ast_unified.t_named_fun (args, f) ~loc *)

let my_ty_expr : Ast_unified.ty_expr =
  Ast_unified.t_var ~loc:Ast_unified.Location.dummy (Ast_unified.Ty_variable.of_input_var "f")


let my_e_unit : Ast_unified.expr = Ast_unified.e_unit ~loc:Ast_unified.Location.dummy

let my_expr : Ast_unified.expr =
  let open Ast_unified in
  let type_in : (expr, ty_expr) Type_in.t =
    { type_binder = "my_binder"
    ; rhs = my_ty_expr
    ; body = my_e_unit
    }
  in
  e_typein ~loc:Location.dummy type_in 


let my_sexp = Sexp.of_string {|
  (E_TypeIn
  ((type_binder my_binder)
    (rhs (T_Var ((name f))))
    (body (E_Literal Literal_unit))))
|}


let () =
  let my_sexp : Sexp.t = Ast_unified.S_exp.sexp_of_expr my_expr in
  Format.printf "Sexp test : %a@." (Sexp.pp_hum_indent 2) my_sexp


let () =
  let my_expr = Ast_unified.S_exp.expr_of_sexp my_sexp in
  let my_sexp : Sexp.t = Ast_unified.S_exp.sexp_of_expr my_expr in
  Format.printf "Sexp test et vice-versa : %a@." (Sexp.pp_hum_indent 2) my_sexp
