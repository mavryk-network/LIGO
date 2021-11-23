open Ast_typed
open Stage_common.Constant

let basic_types : (type_variable * type_expression) list = [
    (v_bool , t_bool ()) ;
    (v_string , t_string ()) ;
    (v_bytes , t_bytes ()) ;
    (v_int , t_int ()) ;
    (v_nat , t_nat ()) ;
    (v_unit , t_unit ()) ;
    (v_option , t_abstraction1 option_name ()) ;
  ]

let michelson_base : (type_variable * type_expression) list = [
    (v_operation , t_operation ()) ;
    (v_tez , t_constant tez_name []) ;
    (v_address , t_address ()) ;
    (v_signature , t_signature ()) ;
    (v_key , t_key ()) ;
    (v_key_hash , t_key_hash ()) ;
    (v_timestamp , t_timestamp ()) ;
    (v_list , t_abstraction1 list_name ()) ;
    (v_big_map , t_abstraction2 big_map_name () ());
    (v_map , t_abstraction2 map_name () ()) ;
    (v_set , t_abstraction1 set_name ());
    (v_contract , t_abstraction1 contract_name ());
    (v_map_or_big_map , t_abstraction2 map_or_big_map_name () ());
    (v_michelson_or , t_abstraction2 michelson_or_name () ());
    (v_michelson_pair , t_abstraction2 michelson_pair_name () ());
    (v_chain_id , t_chain_id ()) ;
    (v_baker_hash , t_baker_hash ()) ;
    (v_pvss_key , t_pvss_key ()) ;
    (v_sapling_state , t_abstraction1 sapling_state_name ()) ;
    (v_sapling_trasaction , t_abstraction1 sapling_transaction_name ()) ;
    (v_baker_operation , t_constant baker_operation_name []) ;
    (v_bls12_381_g1 , t_bls12_381_g1 ()) ;
    (v_bls12_381_g2 , t_bls12_381_g2 ()) ;
    (v_bls12_381_fr ,  t_bls12_381_fr ()) ;
    (v_never , t_never ()) ;
    (v_ticket , t_abstraction1 ticket_name ()) ;
]

let hangzhou_extra : (type_variable * type_expression) list = [
  (v_chest , t_chest ());
  (v_chest_key , t_chest_key ());
  (v_chest_opening_result , t_chest_opening_result ());
]

let edo_types = basic_types @ michelson_base
let hangzhou_types = basic_types @ michelson_base @ hangzhou_extra

