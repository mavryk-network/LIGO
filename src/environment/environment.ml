open Helpers
open Ast_typed
module Protocols = Protocols

open Stage_common.Constant

let meta_ligo_types : (type_variable * type_expression) list -> (type_variable * type_expression) list =
  fun proto_types ->
    proto_types @ [
    (v_test_michelson, t_constant test_michelson_name []) ;
    (v_test_exec_error, t_test_exec_error () ) ;
    (v_test_exec_result , t_test_exec_result () ) ;
    (v_account , t_constant account_name []) ;
    (v_typed_address , t_abstraction2 typed_address_name () ()) ;
    (v_time , t_constant time_name []) ;
    (v_mutation, t_constant mutation_name []);
    (v_failure, t_constant failure_name []);
  ]
let e_raw_code = Ast_typed.Combinators.e_a_raw_michelson_code

(* TODO: organize these in a better way maybe a new file/module *)

let michelson_slice = "{ UNPAIR ;
UNPAIR ;
SLICE ;
IF_NONE { PUSH string \"SLICE\" ; FAILWITH } {} }"

let slice_type = t_function (t_triplet (t_nat ()) (t_nat ()) (t_string ())) (t_string ()) ()

(*

let tezos_module e = make_module e "Tezos" [
  ("chain_id", e_raw_code "{ CHAIN_ID}" );
  ("balance", e_raw_code "{ BALANCE }");
  ("now",);
  ("amount",);
  ("sender",);
  ("address",);
  ("self",);
  ("self_address",);
  ("implicit_account",);
  ("source",);
  ("failwith",);
  ("transaction",);
  ("set_delegate",);
  ("get_contract_with_error",);
  ("get_contract_opt",);
  ("get_entrypoint_opt",);
  ("level",);
  ("pairing_check",);
  ("never",);
  ("open_chet",);
  ("call_view",);

  (* sapling *)
  ("sapling_empty_state", e_raw_code "{ SAPLING_EMPTY_STATE }" );
  ("sapling_verify_update", e_raw_code "{ SAPLNING_VERIFY_UPDATE }" );

  (* Tickets *)
  ("create_ticket", e_raw_code "{TICKET}");
  ("join_tickets", e_raw_code "{JOIN_TICKETS}");
  ("split_ticket", e_raw_code "{SPLIT_TICKETS}");
  ("read_ticket", e_raw_code "{ READ_TICKET; PAIR }");
]

*)

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
] (* Deprecated*)

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


open Michelson

let default : Protocols.t -> environment = function
  | Protocols.Edo -> Environment.of_list_type edo_types |> add_build_in_values
  | Protocols.Hangzhou -> Environment.of_list_type hangzhou_types |> add_build_in_values

let default_with_test : Protocols.t -> environment = function
  | Protocols.Edo -> Environment.of_list_type (meta_ligo_types edo_types) |> add_build_in_values
  | Protocols.Hangzhou -> Environment.of_list_type (meta_ligo_types hangzhou_types) |> add_build_in_values
