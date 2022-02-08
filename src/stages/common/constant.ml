open Types
(* type constants *)
let bool_name                              = "bool"
let string_name                            = "string"
let bytes_name                             = "bytes"
let int_name                               = "int"
let operation_name                         = "operation"
let nat_name                               = "nat"
let tez_name                               = "tez"
let unit_name                              = "unit"
let address_name                           = "address"
let signature_name                         = "signature"
let key_name                               = "key"
let key_hash_name                          = "key_hash"
let timestamp_name                         = "timestamp"
let chain_id_name                          = "chain_id"
let option_name                            = "option"
let list_name                              = "list"
let map_name                               = "map"
let big_map_name                           = "big_map"
let set_name                               = "set"
let contract_name                          = "contract"
let michelson_or_name                      = "michelson_or"
let michelson_pair_name                    = "michelson_pair"
let map_or_big_map_name                    = "map_or_big_map"
let baker_hash_name                        = "baker_hash"
let pvss_key_name                          = "pvss_key"
let sapling_transaction_name               = "sapling_transaction"
let sapling_state_name                     = "sapling_state"
let baker_operation_name                   = "baker_operation"
let bls12_381_g1_name                      = "bls12_381_g1"
let bls12_381_g2_name                      = "bls12_381_g2"
let bls12_381_fr_name                      = "bls12_381_fr"
let never_name                             = "never"
let ticket_name                            = "ticket"
let test_michelson_name                    = "michelson_program"
let account_name                           = "account"
let time_name                              = "time"
let typed_address_name                     = "typed_address"
let mutation_name                          = "mutation"
let failure_name                           = "failure"
let chest_name                             = "chest"
let chest_key_name                         = "chest_key"
let chest_opening_result_name              = "chest_opening_result"
let v_bool                 : type_variable = Var.of_input_var bool_name
let v_string               : type_variable = Var.of_input_var string_name
let v_bytes                : type_variable = Var.of_input_var bytes_name
let v_int                  : type_variable = Var.of_input_var int_name
let v_operation            : type_variable = Var.of_input_var operation_name
let v_nat                  : type_variable = Var.of_input_var nat_name
let v_tez                  : type_variable = Var.of_input_var tez_name
let v_unit                 : type_variable = Var.of_input_var unit_name
let v_address              : type_variable = Var.of_input_var address_name
let v_signature            : type_variable = Var.of_input_var signature_name
let v_key                  : type_variable = Var.of_input_var key_name
let v_key_hash             : type_variable = Var.of_input_var key_hash_name
let v_timestamp            : type_variable = Var.of_input_var timestamp_name
let v_chain_id             : type_variable = Var.of_input_var chain_id_name
let v_option               : type_variable = Var.of_input_var option_name
let v_list                 : type_variable = Var.of_input_var list_name
let v_map                  : type_variable = Var.of_input_var map_name
let v_big_map              : type_variable = Var.of_input_var big_map_name
let v_set                  : type_variable = Var.of_input_var set_name
let v_contract             : type_variable = Var.of_input_var contract_name
let v_michelson_or         : type_variable = Var.of_input_var michelson_or_name
let v_michelson_pair       : type_variable = Var.of_input_var michelson_pair_name
let v_map_or_big_map       : type_variable = Var.of_input_var map_or_big_map_name
let v_baker_hash           : type_variable = Var.of_input_var baker_hash_name
let v_pvss_key             : type_variable = Var.of_input_var pvss_key_name
let v_sapling_trasaction   : type_variable = Var.of_input_var sapling_transaction_name
let v_sapling_state        : type_variable = Var.of_input_var sapling_state_name
let v_baker_operation      : type_variable = Var.of_input_var baker_operation_name
let v_bls12_381_g1         : type_variable = Var.of_input_var bls12_381_g1_name
let v_bls12_381_g2         : type_variable = Var.of_input_var bls12_381_g2_name
let v_bls12_381_fr         : type_variable = Var.of_input_var bls12_381_fr_name
let v_never                : type_variable = Var.of_input_var never_name
let v_ticket               : type_variable = Var.of_input_var ticket_name
let v_test_michelson       : type_variable = Var.of_input_var test_michelson_name
let v_account              : type_variable = Var.of_input_var account_name
let v_time                 : type_variable = Var.of_input_var time_name
let v_typed_address        : type_variable = Var.of_input_var typed_address_name
let v_mutation             : type_variable = Var.of_input_var mutation_name
let v_failure              : type_variable = Var.of_input_var failure_name
let v_chest                : type_variable = Var.of_input_var chest_name
let v_chest_key            : type_variable = Var.of_input_var chest_key_name
let v_chest_opening_result : type_variable = Var.of_input_var chest_opening_result_name
