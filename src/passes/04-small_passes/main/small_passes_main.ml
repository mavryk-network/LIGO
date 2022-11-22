
(* Name shortcuts *)
module SP = Small_passes_lib.Small_passes
module PE = Small_passes_lib.Pass_example
module AST = Ast_unified

(* Ligo_prim dependencies *)
module Label = Ligo_prim.Label

let () = Format.printf "Hello world from small_passes_main@."

(* Some little tests *)

let titi_1 : PE.fix_type_expr =
  `T_Arg ("titi", 42)

let titi_2 : PE.fix_type_expr =
  let rhs : PE.fix_type_expr option AST.Types.Non_linear_rows.row_element =
    { associated_type = Some titi_1
    ; attributes = []
    ; decl_pos = 42
    } in
  let record =
    [ Label.of_string "lhs_toto", rhs
    ] in
  `T_Record_raw (record, 42)


(* Main test *)

type te = PE.fix_type_expr
type te_pass = te SP.pass

let passes_list =
  let open PE in
  [ pass_t_arg
  ; pass_t_named_fun
  ; pass_t_app_michelson_types
  ]

let ghost_loc : 'a -> 'a PE.Loc.t = fun x -> (x, 0)

let tvar_of_str = AST.Types.Ty_variable.of_input_var

let my_fun : te =
  let args : te AST.Named_fun.fun_type_args = 
    [ { name = "arg_1" ; type_expr = `T_Var (ghost_loc @@ tvar_of_str "a1") }
    ; { name = "arg_2" ; type_expr = `T_Var (ghost_loc @@ tvar_of_str "a2") }
    ; { name = "arg_3" ; type_expr = `T_Var (ghost_loc @@ tvar_of_str "a3") }
    ]
  in
  let f : te = `T_Var (ghost_loc @@ tvar_of_str "f") in
  `T_Named_fun (ghost_loc (args, f))

let my_michelson_pair : te =
  let loc = 42 in
  let constr = "michelson_pair" in
  let type_args : te Simple_utils.List.Ne.t =
    ( `T_Var (ghost_loc @@ tvar_of_str "my_first_arg"),
    [ `T_Var (ghost_loc @@ tvar_of_str "my_second_arg")
    ])
  in
  `T_App ({constr; type_args}, loc)

let my_michelson_3uple : te =
  let loc = 42 in
  let constr = "michelson_pair" in
  let type_args : te Simple_utils.List.Ne.t =
    ( `T_Var (ghost_loc @@ tvar_of_str "my_first_arg"),
    [ `T_Var (ghost_loc @@ tvar_of_str "my_second_arg")
    ; `T_Var (ghost_loc @@ tvar_of_str "my_unexpected_3rd_arg")
    ])
  in
  `T_App ({constr; type_args}, loc)

let my_michelson_or : te =
  let loc = 42 in
  let constr = "michelson_or" in
  let type_args : te Simple_utils.List.Ne.t =
    ( `T_Var (ghost_loc @@ tvar_of_str "my_first_arg"),
    [ `T_Var (ghost_loc @@ tvar_of_str "my_second_arg")
    ])
  in
  `T_App ({constr; type_args}, loc)

let my_sapling_state : te =
  let loc = 0 in
  let t_int = `T_Int (ghost_loc ("annot", Z.of_int 42)) in
  let type_args = Simple_utils.List.Ne.singleton t_int in
  let constr = "sapling_state" in
  `T_App ({constr; type_args}, loc)

let my_sapling_state_wrong : te =
  let loc = 0 in
  let not_t_int = `T_Var (ghost_loc @@ tvar_of_str "i_should_be_a_t_int") in
  let type_args = Simple_utils.List.Ne.singleton not_t_int in
  let constr = "sapling_state" in
  `T_App ({constr; type_args}, loc)

let my_sapling_transaction : te =
  let loc = 0 in
  let t_int = `T_Int (ghost_loc ("annot", Z.of_int 42)) in
  let type_args = Simple_utils.List.Ne.singleton t_int in
  let constr = "sapling_transaction" in
  `T_App ({constr; type_args}, loc)

let my_sapling_transaction_wrong : te =
  let loc = 0 in
  let not_t_int = `T_Var (ghost_loc @@ tvar_of_str "i_should_be_a_t_int") in
  let type_args = Simple_utils.List.Ne.singleton not_t_int in
  let constr = "sapling_transaction" in
  `T_App ({constr; type_args}, loc)


let inputs : (string * te) list =
  [ "simple_arg", `T_Arg (ghost_loc "my_arg")
  ; "simple_var", `T_Var (ghost_loc @@ tvar_of_str "my_var")
  ; "named_fun", my_fun
  ; "michelson_pair", my_michelson_pair
  ; "michelson_3uple", my_michelson_3uple
  ; "michelson_or", my_michelson_or
  ; "my_sapling_state", my_sapling_state
  ; "my_sapling_state_wrong", my_sapling_state_wrong
  ; "my_sapling_transaction", my_sapling_transaction
  ; "my_sapling_transaction_wrong", my_sapling_transaction_wrong
  ]

let test_input (passes : te_pass list) (test_name, input : string * te) =
  let ppf = Format.std_formatter in
  let () = Ansi.add_ansi_marking ppf in
  let print_te_with_header ppf header_str te =
    let sexp = PE.sexp_of_fix_type_expr te in
    Format.fprintf ppf "@[<v 2>%s :@,%a@]@."
      header_str
      (Sexplib0.Sexp.pp_hum)
      sexp
  in
  Format.fprintf ppf "@.@{<bold>@{<underline>Test : %s@}@}@." test_name;
  print_te_with_header ppf "Input" input;
  try
    let output = SP.compile_with_passes ~syntax_todo:() passes [] input in
    print_te_with_header ppf "Output" output
  with e ->
    Format.fprintf ppf "Exception : %s@." (Exn.to_string e)


let () = List.iter ~f:(test_input passes_list) inputs

