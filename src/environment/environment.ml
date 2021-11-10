open Ast_typed
open Stage_common.Constant
module Protocols = Protocols
let star = ()

let basic_types : (type_variable * type_expression) list = [
    (v_bool , t_bool ()) ;
    (v_string , t_string ()) ;
    (v_bytes , t_bytes ()) ;
    (v_int , t_int ()) ;
    (v_nat , t_nat ()) ;
    (v_unit , t_unit ()) ;
    (v_option , t_abstraction1 option_name star) ;
  ]

let michelson_base : (type_variable * type_expression) list = [
    (v_operation , t_operation ()) ;
    (v_tez , t_constant tez_name []) ;
    (v_address , t_address ()) ;
    (v_signature , t_signature ()) ;
    (v_key , t_key ()) ;
    (v_key_hash , t_key_hash ()) ;
    (v_timestamp , t_timestamp ()) ;
    (v_list , t_abstraction1 list_name star) ;
    (v_big_map , t_abstraction2 big_map_name star star);
    (v_map , t_abstraction2 map_name star star) ;
    (v_set , t_abstraction1 set_name star);
    (v_contract , t_abstraction1 contract_name star);
    (v_map_or_big_map , t_abstraction2 map_or_big_map_name star star);
    (v_michelson_or , t_abstraction2 michelson_or_name star star);
    (v_michelson_pair , t_abstraction2 michelson_pair_name star star);
    (v_chain_id , t_chain_id ()) ;
    (v_baker_hash , t_baker_hash ()) ;
    (v_pvss_key , t_pvss_key ()) ;
    (v_sapling_state , t_abstraction1 sapling_state_name star) ;
    (v_sapling_trasaction , t_abstraction1 sapling_transaction_name star) ;
    (v_baker_operation , t_constant baker_operation_name []) ;
    (v_bls12_381_g1 , t_bls12_381_g1 ()) ;
    (v_bls12_381_g2 , t_bls12_381_g2 ()) ;
    (v_bls12_381_fr ,  t_bls12_381_fr ()) ;
    (v_never , t_never ()) ;
    (v_ticket , t_abstraction1 ticket_name star) ;
]

let hangzhou_extra : (type_variable * type_expression) list = [
  (v_chest , t_chest ());
  (v_chest_key , t_chest_key ());
  (v_chest_opening_result , t_chest_opening_result ());
]

let edo_types = basic_types @ michelson_base
let hangzhou_types = basic_types @ michelson_base @ hangzhou_extra

let meta_ligo_types : (type_variable * type_expression) list -> (type_variable * type_expression) list =
  fun proto_types ->
    proto_types @ [
    (v_test_michelson, t_constant test_michelson_name []) ;
    (v_test_exec_error, t_test_exec_error () ) ;
    (v_test_exec_result , t_test_exec_result () ) ;
    (v_account , t_constant account_name []) ;
    (v_typed_address , t_abstraction2 typed_address_name star star) ;
    (v_time , t_constant time_name []) ;
    (v_mutation, t_constant mutation_name []);
    (v_failure, t_constant failure_name []);
  ]

let e_raw_code = Ast_typed.Combinators.e_a_raw_michelson_code

let wrap_var s = Location.wrap @@ Var.of_name s

let add_bindings_in_env bs env =
  List.fold_left bs ~init:env ~f:(fun env (v,e) -> 
    let attr = { inline = false ; no_mutation = false; public = true; view = false } in
    Environment.add_ez_declaration ~public:true (wrap_var v) e attr env)

let add_types_in_module_env ts env = 
  List.fold_left ts ~init:env ~f:(fun env (v,t) -> 
    Environment.add_type ~public:true v t env)

let make_module parent_env module_name bindings = 
  let module_env = add_bindings_in_env bindings parent_env in
  Environment.add_module ~public:true module_name module_env parent_env 

(* TODO: organize these in a better way maybe a new file/module *)

let michelson_slice = "{ UNPAIR ;
UNPAIR ;
SLICE ;
IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} }"

let slice_type = t_function (t_triplet (t_nat ()) (t_nat ()) (t_string ())) (t_string ()) ()

let string_module e = make_module e "String" [
  ("length", e_raw_code "{ SIZE }" (t_function (t_string ()) (t_nat ()) ()) ) ;
  ("size"  , e_raw_code "{ SIZE }" (t_function (t_string ()) (t_nat ()) ())) ;
  ("slice" , e_raw_code michelson_slice slice_type) ; (*deprecated*)
  ("sub"   , e_raw_code michelson_slice slice_type) ;
  ("concat", e_raw_code "{ UNPAIR ; CONCAT }" (t_function (t_pair (t_string ()) (t_string ())) (t_string ()) ())) ; 
]

let crypto_module e = make_module e "Crypto" [
  ("blake2b" , e_raw_code "{ BLAKE2B  }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("sha256"  , e_raw_code "{ SHA256   }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("sha512"  , e_raw_code "{ SHA512   }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("sha3"    , e_raw_code "{ SHA3     }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("keccak"  , e_raw_code "{ KECCAK   }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("hash_key", e_raw_code "{ HASH_KEY }" (t_function (t_key   ()) (t_key_hash ()) ())) ;
  ("check"   , e_raw_code "{ UNPAIR ; UNPAIR ; CHECK_SIGNATURE }" (t_function (t_triplet (t_key ()) (t_signature ()) (t_bytes ())) (t_bool ()) ()))
]

let michelson_module e = make_module e "Michelson" [
  ("is_nat", e_raw_code "{ ISNAT }" (t_function (t_int ()) (t_option (t_nat ())) ())) ;
]

let toplevel e = add_bindings_in_env [
  ("blake2b"        , e_raw_code "{ BLAKE2B  }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("sha256"         , e_raw_code "{ SHA256   }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("sha512"         , e_raw_code "{ SHA512   }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("crypto_hash_key", e_raw_code "{ HASH_KEY }" (t_function (t_key   ()) (t_key_hash ()) ())) ;
  ("crypto_check"   , e_raw_code "{ UNPAIR ; UNPAIR ; CHECK_SIGNATURE }" (t_function (t_triplet (t_key ()) (t_signature ()) (t_bytes ())) (t_bool ()) ())) ;

  ("string_slice" , e_raw_code michelson_slice slice_type) ;
  ("string_concat", e_raw_code "{ UNPAIR ; CONCAT }" (t_function (t_pair (t_string ()) (t_string ())) (t_string ()) ())) ; 

  ("is_nat", e_raw_code "{ ISNAT }" (t_function (t_int ()) (t_option (t_nat ())) ())) ;
  ("int"   , e_raw_code "{ INT }" (t_function (t_nat ()) (t_int ()) ())) ;
  ("abs"   , e_raw_code "{ ABS }" (t_function (t_int ()) (t_nat ()) ())) ;

] e

let add_build_in_values e = 
  e
  |> string_module
  |> crypto_module
  |> michelson_module
  |> toplevel

let default : Protocols.t -> environment = function
  | Protocols.Edo -> Environment.of_list_type edo_types |> add_build_in_values
  | Protocols.Hangzhou -> Environment.of_list_type hangzhou_types |> add_build_in_values

let default_with_test : Protocols.t -> environment = function
  | Protocols.Edo -> Environment.of_list_type (meta_ligo_types edo_types) |> add_build_in_values
  | Protocols.Hangzhou -> Environment.of_list_type (meta_ligo_types hangzhou_types) |> add_build_in_values
