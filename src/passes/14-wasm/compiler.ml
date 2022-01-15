(* use https://github.com/SanderSpies/ocaml/blob/manual_gc/asmcomp/wasm32/emit.mlp for inspiration *)

[@@@warning "-33-27"]
open Trace
open Errors 

module I = Mini_c.Types
module W = WasmObjectFile  

let expression ~raise : I.expression -> W.Ast.instr list = fun e ->
  print_endline "oh yes here";
  match e.content with 
  | E_literal (Literal_unit) -> failwith "not supported yet 1"
  | E_literal (Literal_int _z) -> failwith "not supported yet 2"
  | E_literal (Literal_nat _z) -> failwith "not supported yet 3"
  | E_literal (Literal_timestamp _z) -> failwith "not supported yet 4"
  | E_literal (Literal_mutez _z) -> failwith "not supported yet 5"
  | E_literal (Literal_string _s) -> failwith "not supported yet 6"
  | E_literal (Literal_bytes _b) -> failwith "not supported yet 7"
  | E_literal (Literal_address _b) -> failwith "not supported yet 8"
  | E_literal (Literal_signature _b) -> failwith "not supported yet 9"
  | E_literal (Literal_key _b) -> failwith "not supported yet 10"
  | E_literal (Literal_key_hash _b) -> failwith "not supported yet 11"
  | E_literal (Literal_chain_id _b) -> failwith "not supported yet 12"
  | E_literal (Literal_operation _b) -> failwith "not supported yet 13"
  | E_closure {binder; body} -> failwith "not supported yet 14"
  | E_constant _ -> failwith "not supported yet 15"
  | E_application _ -> failwith "not supported yet 16"
  | E_variable _ -> failwith "not supported yet 17"
  | E_iterator _ -> failwith "not supported yet 18"
  | E_fold     _ -> failwith "not supported yet 19"
  | E_fold_right _ -> failwith "not supported yet 20"
  | E_if_bool  _ -> failwith "not supported yet 21"
  | E_if_none  _ -> failwith "not supported yet 22"
  | E_if_cons  _ -> failwith "not supported yet 23"
  | E_if_left  _ -> failwith "not supported yet 24"
  | E_let_in (e1, _inline, ((name, _type), _e2)) -> 
    print_endline (Var.debug (Location.unwrap name));
    


    failwith "not supported yet 25"
  | E_tuple _ -> failwith "not supported yet 26"
  | E_let_tuple _ -> failwith "not supported yet 27"
  (* E_proj (record, index, field_count): we use the field_count to
     know whether the index is the last field or not, since Michelson
     treats the last element of a comb differently than the rest. We
     could alternatively put `unit` at the end of all our combs, but
     that would break compatibility and is not a standard Michelson
     convention... *)
  | E_proj _ -> failwith "not supported yet 28"
  (* E_update (record, index, update, field_count): field_count as for E_proj *)
  | E_update _ -> failwith "not supported yet 29"  
  | E_raw_michelson _ -> raise.raise @@ michelson_insertion Location.dummy
  | _ -> []
  (* | E_closure of anon_function
  | E_constant of constant
  | E_application of (expression * expression)
  | E_variable of var_name
  | E_iterator of constant' * ((var_name * type_expression) * expression) * expression
  | E_fold     of (((var_name * typex _expression) * expression) * expression * expression)
  | E_fold_right of (((var_name * type_expression) * expression) * (expression * type_expression) * expression)
  | E_if_bool  of (expression * expression * expression)
  | E_if_none  of expression * expression * ((var_name * type_expression) * expression)
  | E_if_cons  of expression * expression * (((var_name * type_expression) * (var_name * type_expression)) * expression)
  | E_if_left  of expression * ((var_name * type_expression) * expression) * ((var_name * type_expression) * expression)
  | E_let_in   of expression * inline * ((var_name * type_expression) * expression)
  | E_tuple of expression list
  | E_let_tuple of expression * (((var_name * type_expression) list) * expression)
  (* E_proj (record, index, field_count): we use the field_count to
     know whether the index is the last field or not, since Michelson
     treats the last element of a comb differently than the rest. We
     could alternatively put `unit` at the end of all our combs, but
     that would break compatibility and is not a standard Michelson
     convention... *)
  | E_proj of expression * int * int
  (* E_update (record, index, update, field_count): field_count as for E_proj *)
  | E_update of expression * int * expression * int
  | E_raw_michelson _ -> failwith "not supported" *)

(* let func_type: I.expression -> W.AST.type_ = fun e ->
  let type_expression = e.type_expression in
  let type_content = type_expression.type_content in  
  let _location = type_expression.location in
  match type_content with  *)
  

  (* | T_tuple of type_expression annotated list
  | T_or of (type_expression annotated * type_expression annotated)
  | T_function of (type_expression * type_expression)
  | T_base of type_base
  | T_map of (type_expression * type_expression)
  | T_big_map of (type_expression * type_expression)
  | T_list of type_expression
  | T_set of type_expression
  | T_contract of type_expression
  | T_ticket of type_expression
  | T_sapling_state of Z.t
  | T_sapling_transaction of Z.t
  | T_option of type_expression *)

let compile ~raise : I.expression -> W.Ast.module_' = fun e -> 
  let _ = expression ~raise e in
  W.Ast.empty_module