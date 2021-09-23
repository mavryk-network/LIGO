open Ast_typed
open Stage_common.Constant
module Protocols = Protocols
let star = ()
(*
  Make sure all the type value laoded in the environment have a `Ast_core` value attached to them (`type_meta` field of `type_expression`)
*)
let basic_types : (type_variable * type_expression) list = [
    (v_bool , t_sum_ez [ ("True" ,t_unit ()); ("False",t_unit ()) ] ) ;
    (v_string , t_constant string_name []) ;
    (v_bytes , t_constant bytes_name []) ;
    (v_int , t_constant int_name []) ;
    (v_nat , t_constant nat_name []) ;
    (v_unit , t_constant unit_name []) ;
    (v_option , t_abstraction1 option_name star) ;
  ]

let michelson_base : (type_variable * type_expression) list = [
    (v_operation , t_constant operation_name []) ;
    (v_tez , t_constant tez_name []) ;
    (v_address , t_constant address_name []) ;
    (v_signature , t_constant signature_name []) ;
    (v_key , t_constant key_name []) ;
    (v_key_hash , t_constant key_hash_name []) ;
    (v_timestamp , t_constant timestamp_name []) ;
    (v_list , t_abstraction1 list_name star) ;
    (v_big_map , t_abstraction2 big_map_name star star);
    (v_map , t_abstraction2 map_name star star) ;
    (v_set , t_abstraction1 set_name star);
    (v_contract , t_abstraction1 contract_name star);
    (v_map_or_big_map , t_abstraction2 map_or_big_map_name star star);
    (v_michelson_or , t_abstraction2 michelson_or_name star star);
    (v_michelson_pair , t_abstraction2 michelson_pair_name star star);
    (v_chain_id , t_constant chain_id_name []) ;
    (v_baker_hash , t_constant baker_hash_name []);
    (v_pvss_key , t_constant pvss_key_name []);
    (v_sapling_state , t_abstraction1 sapling_state_name star);
    (v_sapling_trasaction , t_abstraction1 sapling_transaction_name star);
    (v_baker_operation , t_constant baker_operation_name []);
    (v_bls12_381_g1 , t_constant bls12_381_g1_name []);
    (v_bls12_381_g2 , t_constant bls12_381_g2_name []);
    (v_bls12_381_fr ,  t_constant bls12_381_fr_name []);
    (v_never , t_constant never_name []);
    (v_ticket , t_abstraction1 ticket_name star);
]

let edo_types = basic_types @ michelson_base

let meta_ligo_types : (type_variable * type_expression) list =
  edo_types @ [
    (v_test_michelson, t_constant test_michelson_name []) ;
    (v_test_exec_error, t_test_exec_error () ) ;
    (v_test_exec_result , t_test_exec_result () ) ;
    (v_account , t_constant account_name []) ;
    (v_typed_address , t_abstraction2 typed_address_name star star) ;
    (v_time , t_constant time_name []) ;
    (v_mutation, t_constant mutation_name []);
    (v_failure, t_constant failure_name []);
  ]

let wrap_var s = Location.wrap @@ Var.of_name s

let wrap_constant' c = {
  type_value = t_unit () ;
  definition = ED_declaration {
    expression = {
      expression_content= E_constant {
        cons_name = c ;
        arguments = [] ;
      } ;
      location = Location.generated ;
      type_expression = t_unit () ;
    } ;
    free_variables = [] ;
  } ;
}
  

let tezos_module env = Environment.add_module "Tezos" (Environment.of_list_values [
  (wrap_var "amount", wrap_constant' C_AMOUNT)
] Environment.empty) env

let list_module env = Environment.add_module "List" (Environment.of_list_values [
  (wrap_var "map", wrap_constant' C_LIST_MAP)
] Environment.empty) env

let bitwise_module env = Environment.add_module "Bitwise" (Environment.of_list_values [
  (wrap_var "or", wrap_constant' C_OR)
] Environment.empty) env


let default : Protocols.t -> environment = function
  | Protocols.Edo -> Environment.of_list_type edo_types
    |> tezos_module 
    |> list_module
    |> bitwise_module

let default_with_test : Protocols.t -> environment = function
  | Protocols.Edo -> Environment.of_list_type meta_ligo_types
