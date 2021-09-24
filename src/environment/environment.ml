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

let constant_type c =
  match c with
  | C_SENDER             -> t_address ()
  | C_SOURCE             -> t_address ()
  | C_UNIT               -> t_unit ()
  | C_AMOUNT             -> t_mutez ()
  | C_BALANCE            -> t_mutez ()
  | C_CHAIN_ID           -> t_chain_id ()
  | C_LEVEL              -> t_nat ()
  | C_TOTAL_VOTING_POWER -> t_nat ()
  | C_NOW                -> t_timestamp ()
  | C_TRUE               -> t_bool ()
  | C_FALSE              -> t_bool ()
  
  | _                    -> t_unit ()

  (* maybe add lambda in env *)
  (* take number or args *)
  (* imbricated functions *)
let wrap_constant' c = {
  type_value = constant_type c ;
  definition = ED_declaration {
    expression = {
      expression_content= E_constant {
        cons_name = c ;
        arguments = [] ;
      } ;
      location = Location.generated ;
      type_expression = constant_type c ;
    } ;
    free_variables = [] ;
  } ;
}

let make_module name module_env env = 
  Environment.add_module name (Environment.of_list_values module_env Environment.empty) env 

let tezos_module = make_module "Tezos" [
  (wrap_var "chain_id"               , wrap_constant' C_CHAIN_ID) ;
  (wrap_var "balance"                , wrap_constant' C_BALANCE) ;
  (wrap_var "now"                    , wrap_constant' C_NOW) ;
  (wrap_var "amount"                 , wrap_constant' C_AMOUNT) ;
  (wrap_var "sender"                 , wrap_constant' C_SENDER) ;
  (wrap_var "address"                , wrap_constant' C_ADDRESS) ;
  (wrap_var "self"                   , wrap_constant' C_SELF) ;
  (wrap_var "self_address"           , wrap_constant' C_SELF_ADDRESS) ;
  (wrap_var "implicit_account"       , wrap_constant' C_IMPLICIT_ACCOUNT) ;
  (wrap_var "source"                 , wrap_constant' C_SOURCE) ;
  (wrap_var "failwith"               , wrap_constant' C_FAILWITH) ;
  (wrap_var "create_contract"        , wrap_constant' C_CREATE_CONTRACT) ;
  (wrap_var "transaction"            , wrap_constant' C_CALL) ;
  (wrap_var "set_delegate"           , wrap_constant' C_SET_DELEGATE) ;
  (wrap_var "get_contract_with_error", wrap_constant' C_CONTRACT_WITH_ERROR) ;
  (wrap_var "get_contract_opt"       , wrap_constant' C_CONTRACT_OPT) ;
  (wrap_var "get_entrypoint_opt"     , wrap_constant' C_CONTRACT_ENTRYPOINT_OPT) ;
  (wrap_var "level"                  , wrap_constant' C_LEVEL) ;
  (wrap_var "pairing_check"          , wrap_constant' C_PAIRING_CHECK) ;
  (wrap_var "never"                  , wrap_constant' C_NEVER) ;
  (wrap_var "sapling_empty_state"    , wrap_constant' C_SAPLING_EMPTY_STATE) ;
  (wrap_var "sapling_verify_update"  , wrap_constant' C_SAPLING_VERIFY_UPDATE) ;
  (wrap_var "create_ticket"          , wrap_constant' C_TICKET) ;
  (wrap_var "join_tickets"           , wrap_constant' C_JOIN_TICKET) ;
  (wrap_var "split_ticket"           , wrap_constant' C_SPLIT_TICKET) ;
  (wrap_var "read_ticket"            , wrap_constant' C_READ_TICKET) ;
]

let crypto_module = make_module "Crypto" [
  (wrap_var "check"   , wrap_constant' C_CHECK_SIGNATURE ) ;
  (wrap_var "hash_key", wrap_constant' C_HASH_KEY ) ;
  (wrap_var "blake2b" , wrap_constant' C_BLAKE2b ) ;
  (wrap_var "sha256"  , wrap_constant' C_SHA256 ) ;
  (wrap_var "sha512"  , wrap_constant' C_SHA512 ) ;
  (wrap_var "sha3"    , wrap_constant' C_SHA3 ) ;
  (wrap_var "keccak"  , wrap_constant' C_KECCAK ) ;
]

let bytes_module = make_module "Bytes" [
  (wrap_var "pack"  ,  wrap_constant' C_BYTES_PACK) ;
  (wrap_var "unpack",  wrap_constant' C_BYTES_UNPACK) ;
  (wrap_var "length",  wrap_constant' C_SIZE) ;
  (wrap_var "concat",  wrap_constant' C_CONCAT) ;
  (wrap_var "sub"   ,  wrap_constant' C_SLICE) ;

  (wrap_var "size" , wrap_constant' C_SIZE) ;
  (wrap_var "slice", wrap_constant' C_SLICE) ;
]

let list_module = make_module "List" [
  (wrap_var "length"    , wrap_constant' C_SIZE) ;
  (wrap_var "size"      , wrap_constant' C_SIZE) ;
  (wrap_var "iter"      , wrap_constant' C_LIST_ITER) ;
  (wrap_var "map"       , wrap_constant' C_LIST_MAP) ;
  (wrap_var "fold"      , wrap_constant' C_LIST_FOLD) ;
  (wrap_var "fold_left" , wrap_constant' C_LIST_FOLD_LEFT) ;
  (wrap_var "fold_right", wrap_constant' C_LIST_FOLD_RIGHT) ;
  (wrap_var "head_opt"  , wrap_constant' C_LIST_HEAD_OPT) ;
  (wrap_var "tail_opt"  , wrap_constant' C_LIST_TAIL_OPT) ;
]

let bitwise_module = make_module "Bitwise" [
  (wrap_var "or"         , wrap_constant' C_OR) ;
  (wrap_var "and"        , wrap_constant' C_AND) ;
  (wrap_var "xor"        , wrap_constant' C_XOR) ;
  (wrap_var "shift_left" , wrap_constant' C_LSL) ;
  (wrap_var "shift_right", wrap_constant' C_LSR) ;

  (wrap_var "lor" , wrap_constant' C_OR) ; 
  (wrap_var "land", wrap_constant' C_AND) ;
  (wrap_var "lxor", wrap_constant' C_XOR) ;
]

let string_module = make_module "String" [
  (wrap_var "length", wrap_constant' C_SIZE) ;
  (wrap_var "size"  , wrap_constant' C_SIZE) ;
  (wrap_var "slice" , wrap_constant' C_SLICE) ; 
  (wrap_var "sub"   , wrap_constant' C_SLICE) ;
  (wrap_var "concat", wrap_constant' C_CONCAT) ;
]

let option_module = make_module "Option" [
  (wrap_var "unopt"           ,  wrap_constant' C_UNOPT) ;
  (wrap_var "unopt_with_error",  wrap_constant' C_UNOPT_WITH_ERROR) ;
]

let operator_module = make_module "Operator" [
  (wrap_var "neg"    ,  wrap_constant' C_NEG) ;
  (wrap_var "add"    ,  wrap_constant' C_ADD) ; (* maybe new type for these? special typer*)
  (wrap_var "sub"    ,  wrap_constant' C_SUB) ;
  (wrap_var "times"  ,  wrap_constant' C_MUL) ;
  (wrap_var "div"    ,  wrap_constant' C_DIV) ;
  (wrap_var "modulus",  wrap_constant' C_MOD) ;
  (wrap_var "eq"     ,  wrap_constant' C_EQ) ;
  (wrap_var "not"    ,  wrap_constant' C_NOT) ;
  (wrap_var "and"    ,  wrap_constant' C_AND) ;
  (wrap_var "or"     ,  wrap_constant' C_OR) ;
  (wrap_var "gt"     ,  wrap_constant' C_GT) ;
  (wrap_var "ge"     ,  wrap_constant' C_GE) ;
  (wrap_var "lt"     ,  wrap_constant' C_LT) ;
  (wrap_var "le"     ,  wrap_constant' C_LE) ;
  (wrap_var "cons"   ,  wrap_constant' C_CONS) ;
  (wrap_var "neq"    ,  wrap_constant' C_NEQ) ;
]

let set_module = make_module "Set" [
  (wrap_var "empty"    ,  wrap_constant' C_SET_EMPTY) ;
  (wrap_var "literal"  ,  wrap_constant' C_SET_LITERAL) ;
  (wrap_var "cardinal" ,  wrap_constant' C_SIZE) ;
  (wrap_var "mem"      ,  wrap_constant' C_SET_MEM) ;
  (wrap_var "add"      ,  wrap_constant' C_SET_ADD) ;
  (wrap_var "remove"   ,  wrap_constant' C_SET_REMOVE) ;
  (wrap_var "iter"     ,  wrap_constant' C_SET_ITER) ;
  (wrap_var "fold"     ,  wrap_constant' C_SET_FOLD) ;
  (wrap_var "fold_asc" ,  wrap_constant' C_SET_FOLD) ;
  (wrap_var "fold_desc",  wrap_constant' C_SET_FOLD_DESC) ;
  (wrap_var "update"   ,  wrap_constant' C_SET_UPDATE) ;

  (wrap_var "size", wrap_constant' C_SIZE) ;
]

let map_module = make_module "Map" [
  (wrap_var "find_opt"      , wrap_constant' C_MAP_FIND_OPT) ;
  (wrap_var "update"        , wrap_constant' C_MAP_UPDATE) ;
  (wrap_var "iter"          , wrap_constant' C_MAP_ITER) ;
  (wrap_var "map"           , wrap_constant' C_MAP_MAP) ;
  (wrap_var "fold"          , wrap_constant' C_MAP_FOLD) ;
  (wrap_var "mem"           , wrap_constant' C_MAP_MEM) ;
  (wrap_var "size"          , wrap_constant' C_SIZE) ;
  (wrap_var "add"           , wrap_constant' C_MAP_ADD) ;
  (wrap_var "remove"        , wrap_constant' C_MAP_REMOVE) ;
  (wrap_var "empty"         , wrap_constant' C_MAP_EMPTY) ;
  (wrap_var "literal"       , wrap_constant' C_MAP_LITERAL) ;
  (wrap_var "get_and_update", wrap_constant' C_MAP_GET_AND_UPDATE) ;

  (wrap_var "find", wrap_constant' C_MAP_FIND) ;
]

let big_map_module = make_module "Big_map" [
  (wrap_var "find"           , wrap_constant' C_MAP_FIND) ;
  (wrap_var "find_opt"       , wrap_constant' C_MAP_FIND_OPT) ;
  (wrap_var "update"         , wrap_constant' C_MAP_UPDATE) ;
  (wrap_var "literal"        , wrap_constant' C_BIG_MAP_LITERAL) ;
  (wrap_var "empty"          , wrap_constant' C_BIG_MAP_EMPTY) ;
  (wrap_var "mem"            , wrap_constant' C_MAP_MEM) ;
  (wrap_var "remove"         , wrap_constant' C_MAP_REMOVE) ;
  (wrap_var "add"            , wrap_constant' C_MAP_ADD) ;
  (wrap_var "get_and_update" , wrap_constant' C_BIG_MAP_GET_AND_UPDATE) ;
]

let test_module = make_module "Test" [
  (wrap_var "originate"                  , wrap_constant' C_TEST_ORIGINATE);
  (wrap_var "originate_from_file"        , wrap_constant' C_TEST_ORIGINATE_FROM_FILE) ;
  (wrap_var "set_now"                    , wrap_constant' C_TEST_SET_NOW) ;
  (wrap_var "set_source"                 , wrap_constant' C_TEST_SET_SOURCE) ;
  (wrap_var "set_baker"                  , wrap_constant' C_TEST_SET_BAKER) ;
  (wrap_var "transfer_to_contract"       , wrap_constant' C_TEST_EXTERNAL_CALL_TO_CONTRACT) ;
  (wrap_var "transfer_to_contract_exn"   , wrap_constant' C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN) ;
  (wrap_var "transfer"                   , wrap_constant' C_TEST_EXTERNAL_CALL_TO_ADDRESS) ;
  (wrap_var "transfer_exn"               , wrap_constant' C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN) ;
  (wrap_var "get_storage"                , wrap_constant' C_TEST_GET_STORAGE) ;
  (wrap_var "get_storage_of_address"     , wrap_constant' C_TEST_GET_STORAGE_OF_ADDRESS) ;
  (wrap_var "get_balance"                , wrap_constant' C_TEST_GET_BALANCE) ;
  (wrap_var "michelson_equal"            , wrap_constant' C_TEST_MICHELSON_EQUAL) ;
  (wrap_var "log"                        , wrap_constant' C_TEST_LOG) ;
  (wrap_var "reset_state"                , wrap_constant' C_TEST_STATE_RESET) ;
  (wrap_var "bootstrap_contract"         , wrap_constant' C_TEST_BOOTSTRAP_CONTRACT) ;
  (wrap_var "nth_bootstrap_contract"     , wrap_constant' C_TEST_NTH_BOOTSTRAP_CONTRACT) ;
  (wrap_var "nth_bootstrap_account"      , wrap_constant' C_TEST_GET_NTH_BS) ;
  (wrap_var "last_originations"          , wrap_constant' C_TEST_LAST_ORIGINATIONS) ;
  (wrap_var "compile_value"              , wrap_constant' C_TEST_COMPILE_META_VALUE) ;
  (wrap_var "mutate_value"               , wrap_constant' C_TEST_MUTATE_VALUE) ;
  (wrap_var "mutation_test"              , wrap_constant' C_TEST_MUTATION_TEST) ;
  (wrap_var "mutation_test_all"          , wrap_constant' C_TEST_MUTATION_TEST_ALL) ;
  (wrap_var "save_mutation"              , wrap_constant' C_TEST_SAVE_MUTATION) ;
  (wrap_var "run"                        , wrap_constant' C_TEST_RUN) ;
  (wrap_var "eval"                       , wrap_constant' C_TEST_EVAL) ;
  (wrap_var "compile_contract"           , wrap_constant' C_TEST_COMPILE_CONTRACT) ;
  (wrap_var "to_contract"                , wrap_constant' C_TEST_TO_CONTRACT) ;
  (wrap_var "nth_bootstrap_typed_address", wrap_constant' C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS) ;
  (wrap_var "to_entrypoint"              , wrap_constant' C_TEST_TO_ENTRYPOINT) ;
  (wrap_var "to_typed_address"           , wrap_constant' C_TEST_TO_TYPED_ADDRESS) ;
  (wrap_var "set_big_map"                , wrap_constant' C_TEST_SET_BIG_MAP) ;
]

let loop_module = make_module "Loop" [
  (wrap_var "fold_while", wrap_constant' C_FOLD_WHILE) ;    
  (wrap_var "resume"    , wrap_constant' C_FOLD_CONTINUE) ; 
  (wrap_var "stop"      , wrap_constant' C_FOLD_STOP) ;     
]

let current_module = make_module "Current" [
  (wrap_var "balance"         , wrap_constant' C_BALANCE) ;             
  (wrap_var "time"            , wrap_constant' C_NOW) ;                 
  (wrap_var "amount"          , wrap_constant' C_AMOUNT) ;              
  (wrap_var "sender"          , wrap_constant' C_SENDER) ;              
  (wrap_var "address"         , wrap_constant' C_ADDRESS) ;             
  (wrap_var "self_address"    , wrap_constant' C_SELF_ADDRESS) ;        
  (wrap_var "implicit_account", wrap_constant' C_IMPLICIT_ACCOUNT) ;    
  (wrap_var "source"          , wrap_constant' C_SOURCE) ;              
  (wrap_var "failwith"        , wrap_constant' C_FAILWITH) ;            
]

let operation_module = make_module "Operation" [
  (wrap_var "transaction"       , wrap_constant' C_CALL) ;                    
  (wrap_var "set_delegate"      , wrap_constant' C_SET_DELEGATE) ;            
  (wrap_var "get_contract"      , wrap_constant' C_CONTRACT) ;                
  (wrap_var "get_contract_opt"  , wrap_constant' C_CONTRACT_OPT) ;            
  (wrap_var "get_entrypoint"    , wrap_constant' C_CONTRACT_ENTRYPOINT) ;     
  (wrap_var "get_entrypoint_opt", wrap_constant' C_CONTRACT_ENTRYPOINT_OPT) ; 
]

let michelson_module = make_module "Michelson" [
  (wrap_var "is_nat", wrap_constant' C_IS_NAT) ;
]

let top_level e = List.fold_left ~f:(fun e (v,c) -> Environment.add_expr v c e) ~init:e [
  (wrap_var "assert"                ,  wrap_constant' C_ASSERTION) ;
  (wrap_var "assert_with_error"     ,  wrap_constant' C_ASSERTION_WITH_ERROR) ;
  (wrap_var "assert_some"           ,  wrap_constant' C_ASSERT_SOME) ;
  (wrap_var "assert_some_with_error",  wrap_constant' C_ASSERT_SOME_WITH_ERROR) ;
  (wrap_var "true"                  ,  wrap_constant' C_TRUE) ;
  (wrap_var "false"                 ,  wrap_constant' C_FALSE) ;

  

  (wrap_var "chain_id", wrap_constant' C_CHAIN_ID) ;                
  (wrap_var "balance" , wrap_constant' C_BALANCE) ;             
  (wrap_var "time"    , wrap_constant' C_NOW) ;                 
  (wrap_var "amount"  , wrap_constant' C_AMOUNT) ;              
  (wrap_var "sender"  , wrap_constant' C_SENDER) ;              
  (wrap_var "source"  , wrap_constant' C_SOURCE) ;              
  (wrap_var "failwith", wrap_constant' C_FAILWITH) ;
  (wrap_var "is_nat"  , wrap_constant' C_IS_NAT) ;
  (wrap_var "int"     , wrap_constant' C_INT) ;
  (wrap_var "abs"     , wrap_constant' C_ABS) ;
  (wrap_var "ediv"    , wrap_constant' C_EDIV) ;
  (wrap_var "unit"    , wrap_constant' C_UNIT) ;
  (wrap_var "continue", wrap_constant' C_FOLD_CONTINUE) ; 
  (wrap_var "stop"    , wrap_constant' C_FOLD_STOP) ;     

]

let default : Protocols.t -> environment = function
  | Protocols.Edo -> Environment.of_list_type edo_types
    |> tezos_module
    |> crypto_module
    |> bytes_module
    |> list_module
    |> bitwise_module
    |> string_module
    |> option_module
    |> operator_module
    |> set_module
    |> map_module
    |> big_map_module
    |> test_module
    |> michelson_module
    |> operation_module
    |> current_module
    |> loop_module
    |> top_level

let default_with_test : Protocols.t -> environment = function
  | Protocols.Edo -> Environment.of_list_type meta_ligo_types
    |> tezos_module
    |> crypto_module
    |> bytes_module
    |> list_module
    |> bitwise_module
    |> string_module
    |> option_module
    |> operator_module
    |> set_module
    |> map_module
    |> big_map_module
    |> test_module
    |> michelson_module
    |> operation_module
    |> current_module
    |> loop_module
    |> top_level
