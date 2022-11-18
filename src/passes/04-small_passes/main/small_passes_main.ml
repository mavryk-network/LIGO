
(* Name shortcuts *)
module SP = Small_passes_lib.Small_passes
module PE = Small_passes_lib.Pass_example
module AST = Ast_unified

(* Ligo_prim dependencies *)
module Label = Ligo_prim.Label

let () = Format.printf "Hello world from small_passes_main@."

(* Some little tests *)

let titi_1 : PE.fix_type_expr =
  `T_Prod ((), 42)

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
  ]

let inputs : (string * te) list =
  let ghost_loc x = (x, 0) in
  [ "simple_arg", `T_Arg (ghost_loc "my_arg")
  ; "simple_var", `T_Var (ghost_loc @@ AST.Types.Ty_variable.of_input_var "my_var")
  ]

let test_input (passes : te_pass list) (test_name, input : string * te) =
  let print_te_with_header header_str te =
    let sexp = PE.sexp_of_fix_type_expr te in
    Format.fprintf Format.std_formatter "@[<v 2>%s :@,%a@]@."
      header_str
      (Sexplib0.Sexp.pp_hum)
      sexp
  in
  Format.printf "@.Test : %s@." test_name;
  print_te_with_header "Input" input;
  let output = SP.compile_with_passes ~syntax_todo:() passes [] input in
  print_te_with_header "Output" output;
  ()


let () = List.iter ~f:(test_input passes_list) inputs

