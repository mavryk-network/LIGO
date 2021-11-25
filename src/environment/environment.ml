module H=Helpers
open Ast_typed

open Stage_common.Constant
open H

type t = Environment.t

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

(* TODO: organize these in a better way maybe a new file/module *)

(*

let tezos_module e = make_module e "Tezos" [
  ("chain_id", e_a_raw_code "{ CHAIN_ID}" );
  ("balance", e_a_raw_code "{ BALANCE }");
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
  ("sapling_empty_state", e_a_raw_code "{ SAPLING_EMPTY_STATE }" );
  ("sapling_verify_update", e_a_raw_code "{ SAPLNING_VERIFY_UPDATE }" );

  (* Tickets *)
  ("create_ticket", e_a_raw_code "{TICKET}");
  ("join_tickets", e_a_raw_code "{JOIN_TICKETS}");
  ("split_ticket", e_a_raw_code "{SPLIT_TICKETS}");
  ("read_ticket", e_a_raw_code "{ READ_TICKET; PAIR }");
]

*)

let michelson_module e = make_module e "Michelson" [
  ("is_nat", e_a_raw_code "{ ISNAT }" (t_function (t_int ()) (t_option (t_nat ())) ())) ;
] (* Deprecated*)

let toplevel e = add_bindings_in_env [
  ("blake2b"        , e_a_raw_code "{ BLAKE2B  }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("sha256"         , e_a_raw_code "{ SHA256   }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("sha512"         , e_a_raw_code "{ SHA512   }" (t_function (t_bytes ()) (t_bytes    ()) ())) ;
  ("crypto_hash_key", e_a_raw_code "{ HASH_KEY }" (t_function (t_key   ()) (t_key_hash ()) ())) ;
  ("crypto_check"   , e_a_raw_code "{ UNPAIR ; UNPAIR ; CHECK_SIGNATURE }" (t_function (t_triplet (t_key ()) (t_signature ()) (t_bytes ())) (t_bool ()) ())) ;

  ("string_slice" , e_a_raw_code Strings.michelson_slice Strings.slice_type) ;
  ("string_concat", e_a_raw_code "{ UNPAIR ; CONCAT }" (t_function (t_pair (t_string ()) (t_string ())) (t_string ()) ())) ; 

  ("is_nat", e_a_raw_code "{ ISNAT }" (t_function (t_int ()) (t_option (t_nat ())) ())) ;
  ("int"   , e_a_raw_code "{ INT }" (t_function (t_nat ()) (t_int ()) ())) ;
  ("abs"   , e_a_raw_code "{ ABS }" (t_function (t_int ()) (t_nat ()) ())) ;

] e

let add_build_in_values ~curry e = 
  e
  |> Strings.module_ ~curry
  |> Crypto.module_ ~curry
  |> michelson_module
  |> toplevel


open Michelson

let use_protocol : Compiler_options.Protocols.t -> _ = function
  | Compiler_options.Protocols.Edo -> edo_types
  | Compiler_options.Protocols.Hangzhou -> hangzhou_types

let include_test ~test : _ -> _ = fun e ->
  if test then meta_ligo_types e else e

let init_env ?(test=false) ~curry ~protocol () = 
  Environment.of_list_type @@
    include_test ~test 
      @@ use_protocol protocol
  |> add_build_in_values ~curry
