open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ Test#647 assert_none_with_error#646 assert_some_with_error#642 assert_with_error#638 assert_none#635 assert_some#632 assert#629 originate_from_file_and_mutate_all#627 originate_from_file_and_mutate#600 mutation_test_all#574 mutation_test#558 originate_from_file#543 compile_contract_from_file#534 originate#529 originate_contract#520 to_entrypoint#516 michelson_equal#512 transfer_to_contract_exn#509 transfer_to_contract#502 create_chest_key#495 create_chest#492 set_big_map#489 baker_account#486 add_account#483 sign#480 save_mutation#477 mutate_value#474 bootstrap_contract#471 reset_state_at#467 reset_state#463 log#460 transfer_exn#456 transfer#452 get_last_events_from#448 PBT#438 run#437 make_test#427 gen_small#424 gen#423 unset_print_values#422 set_print_values#421 println#420 nl#418 chr#417 read_contract_from_file#414 compile_contract#412 size#409 set_baker#407 set_baker_policy#405 get_storage#403 to_json#398 to_string#396 drop_context#394 save_context#392 restore_context#390 parse_michelson#388 constant_to_michelson_program#386 to_typed_address#384 register_constant#382 register_delegate#380 cast_address#378 get_time#376 bake_until_n_cycle_end#374 decompile#372 new_account#370 random#368 last_originations#365 nth_bootstrap_typed_address#363 get_bootstrap_account#361 nth_bootstrap_account#359 nth_bootstrap_contract#356 get_voting_power#354 eprint#352 print#350 get_balance#348 get_storage_of_address#346 set_source#344 to_contract#342 failwith#340 get_total_voting_power#338 compile_value#336 eval#334 run#331 unforged_ticket#328 pbt_result#327 pbt_test#326 test_baker_policy#325 test_exec_result#324 test_exec_error#323 test_exec_error_balance_too_low#322 ediv#321 assert_none_with_error#318 assert_some_with_error#314 assert_with_error#310 ignore#307 int#306 unit#304 false#303 true#302 is_nat#301 abs#299 assert_none#297 assert_some#294 assert#291 Crypto#289 check#288 hash_key#284 keccak#282 sha3#280 sha512#278 sha256#276 blake2b#274 Bytes#272 sub#271 concat#267 length#264 unpack#262 pack#260 concats#258 Option#256 map#255 unopt_with_error#253 unopt#249 String#246 sub#245 concat#241 concats#238 length#236 List#234 find_opt#233 cons#229 fold_right#226 fold_left#222 fold#218 iter#214 map#211 tail_opt#208 head_opt#204 size#200 length#198 Set#196 fold_desc#195 fold#191 iter#187 update#184 remove#180 add#177 mem#174 literal#171 cardinal#169 size#167 empty#165 Map#164 fold#163 map#159 iter#156 find#153 find_opt#150 get_and_update#147 update#143 remove#139 add#136 mem#132 literal#129 size#127 empty#125 Big_map#124 find#123 find_opt#120 get_and_update#117 update#113 remove#109 add#106 mem#102 literal#99 empty#97 Bitwise#96 shift_right#95 shift_left#92 or#89 xor#86 and#83 Tezos#80 sapling_verify_update#79 emit#76 get_entrypoint#74 get_entrypoint_opt#70 create_contract#68 split_ticket#66 call_view#63 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ a#648 Test#647 assert_none_with_error#646 assert_some_with_error#642 assert_with_error#638 assert_none#635 assert_some#632 assert#629 originate_from_file_and_mutate_all#627 originate_from_file_and_mutate#600 mutation_test_all#574 mutation_test#558 originate_from_file#543 compile_contract_from_file#534 originate#529 originate_contract#520 to_entrypoint#516 michelson_equal#512 transfer_to_contract_exn#509 transfer_to_contract#502 create_chest_key#495 create_chest#492 set_big_map#489 baker_account#486 add_account#483 sign#480 save_mutation#477 mutate_value#474 bootstrap_contract#471 reset_state_at#467 reset_state#463 log#460 transfer_exn#456 transfer#452 get_last_events_from#448 PBT#438 run#437 make_test#427 gen_small#424 gen#423 unset_print_values#422 set_print_values#421 println#420 nl#418 chr#417 read_contract_from_file#414 compile_contract#412 size#409 set_baker#407 set_baker_policy#405 get_storage#403 to_json#398 to_string#396 drop_context#394 save_context#392 restore_context#390 parse_michelson#388 constant_to_michelson_program#386 to_typed_address#384 register_constant#382 register_delegate#380 cast_address#378 get_time#376 bake_until_n_cycle_end#374 decompile#372 new_account#370 random#368 last_originations#365 nth_bootstrap_typed_address#363 get_bootstrap_account#361 nth_bootstrap_account#359 nth_bootstrap_contract#356 get_voting_power#354 eprint#352 print#350 get_balance#348 get_storage_of_address#346 set_source#344 to_contract#342 failwith#340 get_total_voting_power#338 compile_value#336 eval#334 run#331 unforged_ticket#328 pbt_result#327 pbt_test#326 test_baker_policy#325 test_exec_result#324 test_exec_error#323 test_exec_error_balance_too_low#322 ediv#321 assert_none_with_error#318 assert_some_with_error#314 assert_with_error#310 ignore#307 int#306 unit#304 false#303 true#302 is_nat#301 abs#299 assert_none#297 assert_some#294 assert#291 Crypto#289 check#288 hash_key#284 keccak#282 sha3#280 sha512#278 sha256#276 blake2b#274 Bytes#272 sub#271 concat#267 length#264 unpack#262 pack#260 concats#258 Option#256 map#255 unopt_with_error#253 unopt#249 String#246 sub#245 concat#241 concats#238 length#236 List#234 find_opt#233 cons#229 fold_right#226 fold_left#222 fold#218 iter#214 map#211 tail_opt#208 head_opt#204 size#200 length#198 Set#196 fold_desc#195 fold#191 iter#187 update#184 remove#180 add#177 mem#174 literal#171 cardinal#169 size#167 empty#165 Map#164 fold#163 map#159 iter#156 find#153 find_opt#150 get_and_update#147 update#143 remove#139 add#136 mem#132 literal#129 size#127 empty#125 Big_map#124 find#123 find_opt#120 get_and_update#117 update#113 remove#109 add#106 mem#102 literal#99 empty#97 Bitwise#96 shift_right#95 shift_left#92 or#89 xor#86 and#83 Tezos#80 sapling_verify_update#79 emit#76 get_entrypoint#74 get_entrypoint_opt#70 create_contract#68 split_ticket#66 call_view#63 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 14
    [ c#649 a#648 Test#647 assert_none_with_error#646 assert_some_with_error#642 assert_with_error#638 assert_none#635 assert_some#632 assert#629 originate_from_file_and_mutate_all#627 originate_from_file_and_mutate#600 mutation_test_all#574 mutation_test#558 originate_from_file#543 compile_contract_from_file#534 originate#529 originate_contract#520 to_entrypoint#516 michelson_equal#512 transfer_to_contract_exn#509 transfer_to_contract#502 create_chest_key#495 create_chest#492 set_big_map#489 baker_account#486 add_account#483 sign#480 save_mutation#477 mutate_value#474 bootstrap_contract#471 reset_state_at#467 reset_state#463 log#460 transfer_exn#456 transfer#452 get_last_events_from#448 PBT#438 run#437 make_test#427 gen_small#424 gen#423 unset_print_values#422 set_print_values#421 println#420 nl#418 chr#417 read_contract_from_file#414 compile_contract#412 size#409 set_baker#407 set_baker_policy#405 get_storage#403 to_json#398 to_string#396 drop_context#394 save_context#392 restore_context#390 parse_michelson#388 constant_to_michelson_program#386 to_typed_address#384 register_constant#382 register_delegate#380 cast_address#378 get_time#376 bake_until_n_cycle_end#374 decompile#372 new_account#370 random#368 last_originations#365 nth_bootstrap_typed_address#363 get_bootstrap_account#361 nth_bootstrap_account#359 nth_bootstrap_contract#356 get_voting_power#354 eprint#352 print#350 get_balance#348 get_storage_of_address#346 set_source#344 to_contract#342 failwith#340 get_total_voting_power#338 compile_value#336 eval#334 run#331 unforged_ticket#328 pbt_result#327 pbt_test#326 test_baker_policy#325 test_exec_result#324 test_exec_error#323 test_exec_error_balance_too_low#322 ediv#321 assert_none_with_error#318 assert_some_with_error#314 assert_with_error#310 ignore#307 int#306 unit#304 false#303 true#302 is_nat#301 abs#299 assert_none#297 assert_some#294 assert#291 Crypto#289 check#288 hash_key#284 keccak#282 sha3#280 sha512#278 sha256#276 blake2b#274 Bytes#272 sub#271 concat#267 length#264 unpack#262 pack#260 concats#258 Option#256 map#255 unopt_with_error#253 unopt#249 String#246 sub#245 concat#241 concats#238 length#236 List#234 find_opt#233 cons#229 fold_right#226 fold_left#222 fold#218 iter#214 map#211 tail_opt#208 head_opt#204 size#200 length#198 Set#196 fold_desc#195 fold#191 iter#187 update#184 remove#180 add#177 mem#174 literal#171 cardinal#169 size#167 empty#165 Map#164 fold#163 map#159 iter#156 find#153 find_opt#150 get_and_update#147 update#143 remove#139 add#136 mem#132 literal#129 size#127 empty#125 Big_map#124 find#123 find_opt#120 get_and_update#117 update#113 remove#109 add#106 mem#102 literal#99 empty#97 Bitwise#96 shift_right#95 shift_left#92 or#89 xor#86 and#83 Tezos#80 sapling_verify_update#79 emit#76 get_entrypoint#74 get_entrypoint_opt#70 create_contract#68 split_ticket#66 call_view#63 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ d#650 c#649 a#648 Test#647 assert_none_with_error#646 assert_some_with_error#642 assert_with_error#638 assert_none#635 assert_some#632 assert#629 originate_from_file_and_mutate_all#627 originate_from_file_and_mutate#600 mutation_test_all#574 mutation_test#558 originate_from_file#543 compile_contract_from_file#534 originate#529 originate_contract#520 to_entrypoint#516 michelson_equal#512 transfer_to_contract_exn#509 transfer_to_contract#502 create_chest_key#495 create_chest#492 set_big_map#489 baker_account#486 add_account#483 sign#480 save_mutation#477 mutate_value#474 bootstrap_contract#471 reset_state_at#467 reset_state#463 log#460 transfer_exn#456 transfer#452 get_last_events_from#448 PBT#438 run#437 make_test#427 gen_small#424 gen#423 unset_print_values#422 set_print_values#421 println#420 nl#418 chr#417 read_contract_from_file#414 compile_contract#412 size#409 set_baker#407 set_baker_policy#405 get_storage#403 to_json#398 to_string#396 drop_context#394 save_context#392 restore_context#390 parse_michelson#388 constant_to_michelson_program#386 to_typed_address#384 register_constant#382 register_delegate#380 cast_address#378 get_time#376 bake_until_n_cycle_end#374 decompile#372 new_account#370 random#368 last_originations#365 nth_bootstrap_typed_address#363 get_bootstrap_account#361 nth_bootstrap_account#359 nth_bootstrap_contract#356 get_voting_power#354 eprint#352 print#350 get_balance#348 get_storage_of_address#346 set_source#344 to_contract#342 failwith#340 get_total_voting_power#338 compile_value#336 eval#334 run#331 unforged_ticket#328 pbt_result#327 pbt_test#326 test_baker_policy#325 test_exec_result#324 test_exec_error#323 test_exec_error_balance_too_low#322 ediv#321 assert_none_with_error#318 assert_some_with_error#314 assert_with_error#310 ignore#307 int#306 unit#304 false#303 true#302 is_nat#301 abs#299 assert_none#297 assert_some#294 assert#291 Crypto#289 check#288 hash_key#284 keccak#282 sha3#280 sha512#278 sha256#276 blake2b#274 Bytes#272 sub#271 concat#267 length#264 unpack#262 pack#260 concats#258 Option#256 map#255 unopt_with_error#253 unopt#249 String#246 sub#245 concat#241 concats#238 length#236 List#234 find_opt#233 cons#229 fold_right#226 fold_left#222 fold#218 iter#214 map#211 tail_opt#208 head_opt#204 size#200 length#198 Set#196 fold_desc#195 fold#191 iter#187 update#184 remove#180 add#177 mem#174 literal#171 cardinal#169 size#167 empty#165 Map#164 fold#163 map#159 iter#156 find#153 find_opt#150 get_and_update#147 update#143 remove#139 add#136 mem#132 literal#129 size#127 empty#125 Big_map#124 find#123 find_opt#120 get_and_update#117 update#113 remove#109 add#106 mem#102 literal#99 empty#97 Bitwise#96 shift_right#95 shift_left#92 or#89 xor#86 and#83 Tezos#80 sapling_verify_update#79 emit#76 get_entrypoint#74 get_entrypoint_opt#70 create_contract#68 split_ticket#66 call_view#63 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ e#651 a#648 Test#647 assert_none_with_error#646 assert_some_with_error#642 assert_with_error#638 assert_none#635 assert_some#632 assert#629 originate_from_file_and_mutate_all#627 originate_from_file_and_mutate#600 mutation_test_all#574 mutation_test#558 originate_from_file#543 compile_contract_from_file#534 originate#529 originate_contract#520 to_entrypoint#516 michelson_equal#512 transfer_to_contract_exn#509 transfer_to_contract#502 create_chest_key#495 create_chest#492 set_big_map#489 baker_account#486 add_account#483 sign#480 save_mutation#477 mutate_value#474 bootstrap_contract#471 reset_state_at#467 reset_state#463 log#460 transfer_exn#456 transfer#452 get_last_events_from#448 PBT#438 run#437 make_test#427 gen_small#424 gen#423 unset_print_values#422 set_print_values#421 println#420 nl#418 chr#417 read_contract_from_file#414 compile_contract#412 size#409 set_baker#407 set_baker_policy#405 get_storage#403 to_json#398 to_string#396 drop_context#394 save_context#392 restore_context#390 parse_michelson#388 constant_to_michelson_program#386 to_typed_address#384 register_constant#382 register_delegate#380 cast_address#378 get_time#376 bake_until_n_cycle_end#374 decompile#372 new_account#370 random#368 last_originations#365 nth_bootstrap_typed_address#363 get_bootstrap_account#361 nth_bootstrap_account#359 nth_bootstrap_contract#356 get_voting_power#354 eprint#352 print#350 get_balance#348 get_storage_of_address#346 set_source#344 to_contract#342 failwith#340 get_total_voting_power#338 compile_value#336 eval#334 run#331 unforged_ticket#328 pbt_result#327 pbt_test#326 test_baker_policy#325 test_exec_result#324 test_exec_error#323 test_exec_error_balance_too_low#322 ediv#321 assert_none_with_error#318 assert_some_with_error#314 assert_with_error#310 ignore#307 int#306 unit#304 false#303 true#302 is_nat#301 abs#299 assert_none#297 assert_some#294 assert#291 Crypto#289 check#288 hash_key#284 keccak#282 sha3#280 sha512#278 sha256#276 blake2b#274 Bytes#272 sub#271 concat#267 length#264 unpack#262 pack#260 concats#258 Option#256 map#255 unopt_with_error#253 unopt#249 String#246 sub#245 concat#241 concats#238 length#236 List#234 find_opt#233 cons#229 fold_right#226 fold_left#222 fold#218 iter#214 map#211 tail_opt#208 head_opt#204 size#200 length#198 Set#196 fold_desc#195 fold#191 iter#187 update#184 remove#180 add#177 mem#174 literal#171 cardinal#169 size#167 empty#165 Map#164 fold#163 map#159 iter#156 find#153 find_opt#150 get_and_update#147 update#143 remove#139 add#136 mem#132 literal#129 size#127 empty#125 Big_map#124 find#123 find_opt#120 get_and_update#117 update#113 remove#109 add#106 mem#102 literal#99 empty#97 Bitwise#96 shift_right#95 shift_left#92 or#89 xor#86 and#83 Tezos#80 sapling_verify_update#79 emit#76 get_entrypoint#74 get_entrypoint_opt#70 create_contract#68 split_ticket#66 call_view#63 transaction#61 create_ticket#57 get_contract_with_error#54 get_contract#49 get_contract_opt#45 sapling_empty_state#43 constant#42 self#40 set_delegate#38 pairing_check#36 never#34 read_ticket#32 join_tickets#30 implicit_account#28 address#26 voting_power#24 get_min_block_time#22 get_total_voting_power#20 get_chain_id#18 get_self_address#16 get_level#14 get_source#12 get_sender#10 get_now#8 get_amount#6 get_balance#4 option#2 bool#1 failwith#0  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-30

    Variable definitions:
    (a#648 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (abs#299 -> abs)
    Range: File "", line 316, characters 4-7
    Body Range: File "", line 316, characters 26-69
    Content: |core: int -> nat|
    references: File "", line 641, characters 33-36
    (assert#291 -> assert)
    Range: File "", line 313, characters 4-10
    Body Range: File "", line 313, characters 31-76
    Content: |core: bool -> unit|
    references: []
    (assert_none#297 -> assert_none)
    Range: File "", line 315, characters 4-15
    Body Range: File "", line 315, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_none_with_error#318 -> assert_none_with_error)
    Range: File "", line 334, characters 4-26
    Body Range: File "", line 334, characters 27-35
    Content: |core: ∀ a : * . ( option (a) * string ) -> unit|
    references: []
    (assert_some#294 -> assert_some)
    Range: File "", line 314, characters 4-15
    Body Range: File "", line 314, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_some_with_error#314 -> assert_some_with_error)
    Range: File "", line 333, characters 4-26
    Body Range: File "", line 333, characters 27-35
    Content: |core: ∀ a : * . ( option (a) * string ) -> unit|
    references: []
    (assert_with_error#310 -> assert_with_error)
    Range: File "", line 332, characters 4-21
    Body Range: File "", line 332, characters 49-77
    Content: |unresolved|
    references: []
    (b#652 -> b)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    Content: |resolved: list (int)|
    references: []
    (c#649 -> c)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (d#650 -> d)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#651 -> e)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 13-14
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    (ediv#321 -> ediv)
    Range: File "", line 335, characters 4-8
    Body Range: File "", line 335, characters 9-19
    Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> external_u_ediv (a ,
    b)|
    references: []
    (failwith#0 -> failwith)
    Range: File "", line 3, characters 4-12
    Body Range: File "", line 3, characters 13-23
    Content: |unresolved|
    references:
      File "", line 39, characters 27-35 ,
      File "", line 78, characters 27-35 ,
      File "", line 102, characters 27-35 ,
      File "", line 263, characters 79-87 ,
      File "", line 271, characters 106-114 ,
      File "", line 313, characters 49-57 ,
      File "", line 314, characters 72-80 ,
      File "", line 315, characters 87-95 ,
      File "", line 332, characters 67-75 ,
      File "", line 333, characters 97-105 ,
      File "", line 334, characters 112-120
    (false#303 -> false)
    Range: File "", line 319, characters 4-9
    Body Range: File "", line 319, characters 19-24
    Content: |core: bool|
    references:
      File "", line 392, characters 51-56 ,
      File "", line 436, characters 90-95 ,
      File "", line 439, characters 62-67
    (ignore#307 -> ignore)
    Range: File "", line 322, characters 4-10
    Body Range: File "", line 322, characters 11-19
    Content: |core: ∀ a : * . a -> unit|
    references: []
    (int#306 -> int)
    Range: File "", line 321, characters 4-7
    Body Range: File "", line 321, characters 8-16
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 388, characters 97-100 ,
      File "", line 424, characters 79-82 ,
      File "", line 426, characters 78-81 ,
      File "", line 428, characters 72-75
    (is_nat#301 -> is_nat)
    Range: File "", line 317, characters 4-10
    Body Range: File "", line 317, characters 36-88
    Content: |core: int -> option (nat)|
    references: []
    (true#302 -> true)
    Range: File "", line 318, characters 4-8
    Body Range: File "", line 318, characters 18-22
    Content: |core: bool|
    references:
      File "", line 435, characters 88-92 ,
      File "", line 440, characters 68-72
    (unit#304 -> unit)
    Range: File "", line 320, characters 4-8
    Body Range: File "", line 320, characters 18-38
    Content: |core: unit|
    references: []
    Type definitions:
    (bool#1 -> bool)
    Range: File "", line 5, characters 5-9
    Body Range: File "", line 5, characters 12-24
    Content: : |sum[False -> unit , True -> unit]|
    references:
      File "", line 27, characters 63-67 ,
      File "", line 144, characters 53-57 ,
      File "", line 174, characters 49-53 ,
      File "", line 205, characters 42-46 ,
      File "", line 208, characters 39-43 ,
      File "", line 240, characters 41-45 ,
      File "", line 308, characters 52-56 ,
      File "", line 313, characters 16-20 ,
      File "", line 318, characters 11-15 ,
      File "", line 319, characters 12-16 ,
      File "", line 332, characters 32-36 ,
      File "", line 355, characters 41-45 ,
      File "", line 457, characters 55-59 ,
      File "", line 636, characters 75-79 ,
      File "", line 725, characters 18-22 ,
      File "", line 736, characters 34-38
    (option#2 -> option)
    Range: File "", line 6, characters 8-14
    Body Range: File "", line 6, characters 0-34
    Content: : |funtype 'a : * . option ('a)|
    references:
      File "", line 23, characters 56-73 ,
      File "", line 28, characters 24-39 ,
      File "", line 30, characters 12-20 ,
      File "", line 35, characters 67-86 ,
      File "", line 83, characters 50-67 ,
      File "", line 91, characters 76-84 ,
      File "", line 93, characters 66-94 ,
      File "", line 95, characters 89-104 ,
      File "", line 97, characters 78-95 ,
      File "", line 106, characters 121-165 ,
      File "", line 147, characters 41-49 ,
      File "", line 148, characters 49-57 ,
      File "", line 148, characters 78-86 ,
      File "", line 149, characters 58-66 ,
      File "", line 177, characters 41-49 ,
      File "", line 178, characters 49-57 ,
      File "", line 178, characters 74-82 ,
      File "", line 179, characters 54-62 ,
      File "", line 219, characters 40-48 ,
      File "", line 220, characters 40-55 ,
      File "", line 240, characters 59-67 ,
      File "", line 241, characters 30-38 ,
      File "", line 263, characters 26-34 ,
      File "", line 271, characters 43-51 ,
      File "", line 272, characters 47-55 ,
      File "", line 272, characters 60-68 ,
      File "", line 280, characters 36-44 ,
      File "", line 314, characters 30-38 ,
      File "", line 315, characters 30-38 ,
      File "", line 317, characters 23-33 ,
      File "", line 333, characters 46-54 ,
      File "", line 334, characters 46-54 ,
      File "", line 421, characters 22-35 ,
      File "", line 609, characters 140-153 ,
      File "", line 610, characters 135-148 ,
      File "", line 615, characters 93-109 ,
      File "", line 618, characters 49-70 ,
      File "", line 619, characters 51-64 ,
      File "", line 622, characters 47-57 ,
      File "", line 628, characters 14-27 ,
      File "", line 633, characters 14-27 ,
      File "", line 654, characters 96-106 ,
      File "", line 661, characters 62-83 ,
      File "", line 664, characters 37-58 ,
      File "", line 685, characters 177-198 ,
      File "", line 691, characters 96-106 ,
      File "", line 694, characters 37-58 ,
      File "", line 710, characters 96-106 ,
      File "", line 726, characters 32-40 ,
      File "", line 727, characters 32-40 ,
      File "", line 737, characters 48-56 ,
      File "", line 738, characters 48-56
    (pbt_result#327 -> pbt_result)
    Range: File "", line 356, characters 8-18
    Body Range: File "", line 356, characters 0-41
    Content: : |funtype 'a : * . sum[Fail -> 'a , Success -> unit]|
    references:
      File "", line 458, characters 56-68 ,
      File "", line 459, characters 37-49 ,
      File "", line 461, characters 82-94 ,
      File "", line 465, characters 94-106 ,
      File "", line 468, characters 66-78
    (pbt_test#326 -> pbt_test)
    Range: File "", line 355, characters 8-16
    Body Range: File "", line 355, characters 0-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 457, characters 64-74 ,
      File "", line 458, characters 36-46
    (test_baker_policy#325 -> test_baker_policy)
    Range: File "", line 350, characters 5-22
    Body Range: File "", line 351, character 4 to line 353, character 29
    Content: : |sum[By_account -> address , By_round -> int , Excluding -> list (address)]|
    references: File "", line 414, characters 29-46
    (test_exec_error#323 -> test_exec_error)
    Range: File "", line 343, characters 5-20
    Body Range: File "", line 344, character 4 to line 346, character 19
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low , Other -> string , Rejected -> ( michelson_program * address )]|
    references: File "", line 348, characters 49-64
    (test_exec_error_balance_too_low#322 -> test_exec_error_balance_too_low)
    Range: File "", line 340, characters 5-36
    Body Range: File "", line 341, characters 2-79
    Content: : |record[contract_balance -> tez , contract_too_low -> address , spend_request -> tez]|
    references: File "", line 345, characters 23-54
    (test_exec_result#324 -> test_exec_result)
    Range: File "", line 348, characters 5-21
    Body Range: File "", line 348, characters 24-64
    Content: : |sum[Fail -> test_exec_error , Success -> nat]|
    references:
      File "", line 609, characters 65-81 ,
      File "", line 626, characters 73-89
    (unforged_ticket#328 -> unforged_ticket)
    Range: File "", line 358, characters 8-23
    Body Range: File "", line 358, characters 0-91
    Content: : |funtype 's : * . record[amount -> nat , ticketer -> address , value -> 's]|
    references: []
    Module definitions:
    (Big_map#124 -> Big_map)
    Range: File "", line 129, characters 7-14
    Body Range: File "", line 129, character 0 to line 153, character 3
    Content: Members: Variable definitions:
                      (add#106 -> add)
                      Range: File "", line 145, characters 6-9
                      Body Range: File "", line 145, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v * big_map (k , v) ) -> big_map (k ,
                      v)|
                      references: []
                      (empty#97 -> empty)
                      Range: File "", line 130, characters 16-21
                      Body Range: File "", line 130, characters 22-32
                      Content: |core: ∀ k : * . ∀ v : * . big_map (k ,
                      v)|
                      references: []
                      (find#123 -> find)
                      Range: File "", line 150, characters 6-10
                      Body Range: File "", line 150, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . ( k * big_map (k , v) ) -> v|
                      references: []
                      (find_opt#120 -> find_opt)
                      Range: File "", line 149, characters 6-14
                      Body Range: File "", line 149, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . ( k * big_map (k , v) ) -> option (v)|
                      references: []
                      (get_and_update#117 -> get_and_update)
                      Range: File "", line 148, characters 6-20
                      Body Range: File "", line 148, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . ( k * option (v) * big_map (k , v) ) ->
                      ( option (v) * big_map (k , v) )|
                      references: []
                      (literal#99 -> literal)
                      Range: File "", line 131, characters 25-32
                      Body Range: File "", line 131, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> big_map (k ,
                      v)|
                      references: []
                      (mem#102 -> mem)
                      Range: File "", line 144, characters 6-9
                      Body Range: File "", line 144, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . ( k * big_map (k , v) ) -> bool|
                      references: []
                      (remove#109 -> remove)
                      Range: File "", line 146, characters 6-12
                      Body Range: File "", line 146, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . ( k * big_map (k , v) ) -> big_map (k ,
                      v)|
                      references: []
                      (update#113 -> update)
                      Range: File "", line 147, characters 6-12
                      Body Range: File "", line 147, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . ( k * option (v) * big_map (k , v) ) -> big_map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bitwise#96 -> Bitwise)
    Range: File "", line 111, characters 7-14
    Body Range: File "", line 111, character 0 to line 127, character 3
    Content: Members: Variable definitions:
                      (and#83 -> and)
                      Range: File "", line 121, characters 6-10
                      Body Range: File "", line 121, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> external_u_and (a ,
                      b)|
                      references: []
                      (or#89 -> or)
                      Range: File "", line 123, characters 6-9
                      Body Range: File "", line 123, characters 39-63
                      Content: |core: ( nat * nat ) -> nat|
                      references: []
                      (shift_left#92 -> shift_left)
                      Range: File "", line 124, characters 6-16
                      Body Range: File "", line 124, characters 46-71
                      Content: |core: ( nat * nat ) -> nat|
                      references: []
                      (shift_right#95 -> shift_right)
                      Range: File "", line 125, characters 6-17
                      Body Range: File "", line 125, characters 47-72
                      Content: |core: ( nat * nat ) -> nat|
                      references: []
                      (xor#86 -> xor)
                      Range: File "", line 122, characters 6-9
                      Body Range: File "", line 122, characters 39-64
                      Content: |core: ( nat * nat ) -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bytes#272 -> Bytes)
    Range: File "", line 277, characters 7-12
    Body Range: File "", line 277, character 0 to line 293, character 3
    Content: Members: Variable definitions:
                      (concat#267 -> concat)
                      Range: File "", line 289, characters 6-12
                      Body Range: File "", line 289, characters 50-80
                      Content: |core: ( bytes * bytes ) -> bytes|
                      references: []
                      (concats#258 -> concats)
                      Range: File "", line 278, characters 6-13
                      Body Range: File "", line 278, characters 42-69
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      (length#264 -> length)
                      Range: File "", line 281, characters 6-12
                      Body Range: File "", line 281, characters 33-56
                      Content: |core: bytes -> nat|
                      references: []
                      (pack#260 -> pack)
                      Range: File "", line 279, characters 6-10
                      Body Range: File "", line 279, characters 11-19
                      Content: |core: ∀ a : * . a -> bytes|
                      references: []
                      (sub#271 -> sub)
                      Range: File "", line 290, characters 6-9
                      Body Range: File "", line 290, characters 52-82
                      Content: |core: ( nat * nat * bytes ) -> bytes|
                      references: []
                      (unpack#262 -> unpack)
                      Range: File "", line 280, characters 6-12
                      Body Range: File "", line 280, characters 13-21
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Crypto#289 -> Crypto)
    Range: File "", line 295, characters 7-13
    Body Range: File "", line 295, character 0 to line 311, character 3
    Content: Members: Variable definitions:
                      (blake2b#274 -> blake2b)
                      Range: File "", line 296, characters 6-13
                      Body Range: File "", line 296, characters 36-87
                      Content: |core: bytes -> bytes|
                      references: []
                      (check#288 -> check)
                      Range: File "", line 308, characters 6-11
                      Body Range: File "", line 308, characters 59-161
                      Content: |core: ( key * signature * bytes ) -> bool|
                      references: []
                      (hash_key#284 -> hash_key)
                      Range: File "", line 301, characters 6-14
                      Body Range: File "", line 301, characters 38-91
                      Content: |core: key -> key_hash|
                      references: []
                      (keccak#282 -> keccak)
                      Range: File "", line 300, characters 6-12
                      Body Range: File "", line 300, characters 35-85
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha256#276 -> sha256)
                      Range: File "", line 297, characters 6-12
                      Body Range: File "", line 297, characters 35-85
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha3#280 -> sha3)
                      Range: File "", line 299, characters 6-10
                      Body Range: File "", line 299, characters 33-81
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha512#278 -> sha512)
                      Range: File "", line 298, characters 6-12
                      Body Range: File "", line 298, characters 35-85
                      Content: |core: bytes -> bytes|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (List#234 -> List)
    Range: File "", line 216, characters 7-11
    Body Range: File "", line 216, character 0 to line 244, character 3
    Content: Members: Variable definitions:
                      (cons#229 -> cons)
                      Range: File "", line 239, characters 6-10
                      Body Range: File "", line 239, characters 11-19
                      Content: |core: ∀ a : * . ( a * list (a) ) -> list (a)|
                      references: []
                      (find_opt#233 -> find_opt)
                      Range: File "", line 240, characters 6-14
                      Body Range: File "", line 240, characters 15-23
                      Content: |core: ∀ a : * . ( a -> bool * list (a) ) -> option (a)|
                      references: []
                      (fold#218 -> fold)
                      Range: File "", line 236, characters 6-10
                      Body Range: File "", line 236, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( ( b * a ) -> b * list (a) * b ) -> b|
                      references: File "", line 608, characters 9-13
                      (fold_left#222 -> fold_left)
                      Range: File "", line 237, characters 6-15
                      Body Range: File "", line 237, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( ( b * a ) -> b * b * list (a) ) -> b|
                      references: []
                      (fold_right#226 -> fold_right)
                      Range: File "", line 238, characters 6-16
                      Body Range: File "", line 238, characters 17-27
                      Content: |core: ∀ a : * . ∀ b : * . ( ( a * b ) -> b * list (a) * b ) -> b|
                      references: File "", line 241, characters 4-14
                      (head_opt#204 -> head_opt)
                      Range: File "", line 219, characters 6-14
                      Body Range: File "", line 219, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      (iter#214 -> iter)
                      Range: File "", line 235, characters 6-10
                      Body Range: File "", line 235, characters 11-19
                      Content: |core: ∀ a : * . ( a -> unit * list (a) ) -> unit|
                      references: []
                      (length#198 -> length)
                      Range: File "", line 217, characters 6-12
                      Body Range: File "", line 217, characters 13-21
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (map#211 -> map)
                      Range: File "", line 234, characters 6-9
                      Body Range: File "", line 234, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . ( a -> b * list (a) ) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10
                      (size#200 -> size)
                      Range: File "", line 218, characters 6-10
                      Body Range: File "", line 218, characters 11-19
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (tail_opt#208 -> tail_opt)
                      Range: File "", line 220, characters 6-14
                      Body Range: File "", line 220, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 608, characters 4-8

    (Map#164 -> Map)
    Range: File "", line 155, characters 7-10
    Body Range: File "", line 155, character 0 to line 186, character 3
    Content: Members: Variable definitions:
                      (add#136 -> add)
                      Range: File "", line 175, characters 6-9
                      Body Range: File "", line 175, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v * map (k , v) ) -> map (k ,
                      v)|
                      references: []
                      (empty#125 -> empty)
                      Range: File "", line 156, characters 6-11
                      Body Range: File "", line 156, characters 12-22
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v)|
                      references: []
                      (find#153 -> find)
                      Range: File "", line 180, characters 6-10
                      Body Range: File "", line 180, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . ( k * map (k , v) ) -> v|
                      references: []
                      (find_opt#150 -> find_opt)
                      Range: File "", line 179, characters 6-14
                      Body Range: File "", line 179, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . ( k * map (k , v) ) -> option (v)|
                      references: []
                      (fold#163 -> fold)
                      Range: File "", line 183, characters 6-10
                      Body Range: File "", line 183, characters 11-23
                      Content: |core: ∀ k : * . ∀ v : * . ∀ c : * .
                      ( ( c * ( k * v ) ) -> c * map (k , v) * c ) -> c|
                      references: []
                      (get_and_update#147 -> get_and_update)
                      Range: File "", line 178, characters 6-20
                      Body Range: File "", line 178, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . ( k * option (v) * map (k , v) ) ->
                      ( option (v) * map (k , v) )|
                      references: []
                      (iter#156 -> iter)
                      Range: File "", line 181, characters 6-10
                      Body Range: File "", line 181, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . ( ( k * v ) -> unit * map (k , v) ) -> unit|
                      references: []
                      (literal#129 -> literal)
                      Range: File "", line 158, characters 25-32
                      Body Range: File "", line 158, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> map (k ,
                      v)|
                      references: []
                      (map#159 -> map)
                      Range: File "", line 182, characters 6-9
                      Body Range: File "", line 182, characters 10-22
                      Content: |core: ∀ k : * . ∀ v : * . ∀ w : * .
                      ( ( k * v ) -> w * map (k , v) ) -> map (k ,
                      w)|
                      references: []
                      (mem#132 -> mem)
                      Range: File "", line 174, characters 6-9
                      Body Range: File "", line 174, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . ( k * map (k , v) ) -> bool|
                      references: []
                      (remove#139 -> remove)
                      Range: File "", line 176, characters 6-12
                      Body Range: File "", line 176, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . ( k * map (k , v) ) -> map (k ,
                      v)|
                      references: []
                      (size#127 -> size)
                      Range: File "", line 157, characters 6-10
                      Body Range: File "", line 157, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v) -> nat|
                      references: []
                      (update#143 -> update)
                      Range: File "", line 177, characters 6-12
                      Body Range: File "", line 177, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . ( k * option (v) * map (k , v) ) -> map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Option#256 -> Option)
    Range: File "", line 262, characters 7-13
    Body Range: File "", line 262, character 0 to line 275, character 3
    Content: Members: Variable definitions:
                      (map#255 -> map)
                      Range: File "", line 272, characters 15-18
                      Body Range: File "", line 272, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . ( a -> b * option (a) ) -> option (b)|
                      references: []
                      (unopt#249 -> unopt)
                      Range: File "", line 263, characters 6-11
                      Body Range: File "", line 263, characters 12-20
                      Content: |core: ∀ a : * . option (a) -> a|
                      references: []
                      (unopt_with_error#253 -> unopt_with_error)
                      Range: File "", line 271, characters 6-22
                      Body Range: File "", line 271, characters 23-31
                      Content: |core: ∀ a : * . ( option (a) * string ) -> a|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Set#196 -> Set)
    Range: File "", line 188, characters 7-10
    Body Range: File "", line 188, character 0 to line 214, character 3
    Content: Members: Variable definitions:
                      (add#177 -> add)
                      Range: File "", line 206, characters 6-9
                      Body Range: File "", line 206, characters 10-18
                      Content: |core: ∀ a : * . ( a * set (a) ) -> set (a)|
                      references: []
                      (cardinal#169 -> cardinal)
                      Range: File "", line 191, characters 6-14
                      Body Range: File "", line 191, characters 15-23
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (empty#165 -> empty)
                      Range: File "", line 189, characters 6-11
                      Body Range: File "", line 189, characters 12-20
                      Content: |core: ∀ a : * . set (a)|
                      references: []
                      (fold#191 -> fold)
                      Range: File "", line 210, characters 6-10
                      Body Range: File "", line 210, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( ( b * a ) -> b * set (a) * b ) -> b|
                      references: []
                      (fold_desc#195 -> fold_desc)
                      Range: File "", line 211, characters 6-15
                      Body Range: File "", line 211, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( ( a * b ) -> b * set (a) * b ) -> b|
                      references: []
                      (iter#187 -> iter)
                      Range: File "", line 209, characters 6-10
                      Body Range: File "", line 209, characters 11-19
                      Content: |core: ∀ a : * . ( a -> unit * set (a) ) -> unit|
                      references: []
                      (literal#171 -> literal)
                      Range: File "", line 192, characters 25-32
                      Body Range: File "", line 192, characters 33-41
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      (mem#174 -> mem)
                      Range: File "", line 205, characters 6-9
                      Body Range: File "", line 205, characters 10-18
                      Content: |core: ∀ a : * . ( a * set (a) ) -> bool|
                      references: []
                      (remove#180 -> remove)
                      Range: File "", line 207, characters 6-12
                      Body Range: File "", line 207, characters 13-21
                      Content: |core: ∀ a : * . ( a * set (a) ) -> set (a)|
                      references: []
                      (size#167 -> size)
                      Range: File "", line 190, characters 6-10
                      Body Range: File "", line 190, characters 11-19
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (update#184 -> update)
                      Range: File "", line 208, characters 6-12
                      Body Range: File "", line 208, characters 13-21
                      Content: |unresolved|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (String#246 -> String)
    Range: File "", line 246, characters 7-13
    Body Range: File "", line 246, character 0 to line 260, character 3
    Content: Members: Variable definitions:
                      (concat#241 -> concat)
                      Range: File "", line 256, characters 6-12
                      Body Range: File "", line 256, characters 53-83
                      Content: |core: ( string * string ) -> string|
                      references: []
                      (concats#238 -> concats)
                      Range: File "", line 248, characters 6-13
                      Body Range: File "", line 248, characters 44-71
                      Content: |core: list (string) -> string|
                      references: []
                      (length#236 -> length)
                      Range: File "", line 247, characters 6-12
                      Body Range: File "", line 247, characters 34-57
                      Content: |core: string -> nat|
                      references:
                        File "", line 638, characters 22-28 ,
                        File "", line 641, characters 45-51
                      (sub#245 -> sub)
                      Range: File "", line 257, characters 6-9
                      Body Range: File "", line 257, characters 54-84
                      Content: |core: ( nat * nat * string ) -> string|
                      references:
                        File "", line 639, characters 24-27 ,
                        File "", line 641, characters 23-26
                      Type definitions:
                      Module definitions:

    references:
      File "", line 638, characters 15-21 ,
      File "", line 639, characters 17-23 ,
      File "", line 641, characters 16-22 ,
      File "", line 641, characters 38-44

    (Test#647 -> Test)
    Range: File "", line 360, characters 7-11
    Body Range: File "", line 360, character 0 to line 741, character 3
    Content: Members: Variable definitions:
                      (add_account#483 -> add_account)
                      Range: File "", line 621, characters 6-17
                      Body Range: File "", line 621, characters 51-89
                      Content: |core: ( string * key ) -> unit|
                      references: []
                      (assert#629 -> assert)
                      Range: File "", line 725, characters 6-12
                      Body Range: File "", line 725, characters 33-78
                      Content: |core: bool -> unit|
                      references: []
                      (assert_none#635 -> assert_none)
                      Range: File "", line 727, characters 6-17
                      Body Range: File "", line 727, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_none_with_error#646 -> assert_none_with_error)
                      Range: File "", line 738, characters 6-28
                      Body Range: File "", line 738, characters 29-37
                      Content: |core: ∀ a : * . ( option (a) * string ) -> unit|
                      references: []
                      (assert_some#632 -> assert_some)
                      Range: File "", line 726, characters 6-17
                      Body Range: File "", line 726, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_some_with_error#642 -> assert_some_with_error)
                      Range: File "", line 737, characters 6-28
                      Body Range: File "", line 737, characters 29-37
                      Content: |core: ∀ a : * . ( option (a) * string ) -> unit|
                      references: []
                      (assert_with_error#638 -> assert_with_error)
                      Range: File "", line 736, characters 6-23
                      Body Range: File "", line 736, characters 51-79
                      Content: |unresolved|
                      references: []
                      (bake_until_n_cycle_end#374 -> bake_until_n_cycle_end)
                      Range: File "", line 396, characters 6-28
                      Body Range: File "", line 396, characters 48-94
                      Content: |core: nat -> unit|
                      references: []
                      (baker_account#486 -> baker_account)
                      Range: File "", line 622, characters 6-19
                      Body Range: File "", line 622, characters 68-108
                      Content: |core: ( ( string * key ) * option (tez) ) -> unit|
                      references: []
                      (bootstrap_contract#471 -> bootstrap_contract)
                      Range: File "", line 617, characters 6-24
                      Body Range: File "", line 617, characters 25-35
                      Content: |core: ∀ p : * . ∀ s : * . ( ( p * s ) -> ( list (operation) * s ) * s * tez ) -> unit|
                      references: []
                      (cast_address#378 -> cast_address)
                      Range: File "", line 398, characters 6-18
                      Body Range: File "", line 398, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references: File "", line 651, characters 35-47
                      (chr#417 -> chr)
                      Range: File "", line 421, characters 6-9
                      Body Range: File "", line 422, character 4 to line 430, character 10
                      Content: |core: nat -> option (string)|
                      references: []
                      (compile_contract#412 -> compile_contract)
                      Range: File "", line 417, characters 6-22
                      Body Range: File "", line 417, characters 23-33
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) * s ) -> michelson_contract|
                      references: File "", line 647, characters 12-28
                      (compile_contract_from_file#534 -> compile_contract_from_file)
                      Range: File "", line 653, characters 6-32
                      Body Range: File "", line 654, character 4 to line 655, character 52
                      Content: |core: ( string * string * list (string) ) -> michelson_contract|
                      references: File "", line 657, characters 12-38
                      (compile_value#336 -> compile_value)
                      Range: File "", line 374, characters 6-19
                      Body Range: File "", line 374, characters 20-28
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      (constant_to_michelson_program#386 -> constant_to_michelson_program)
                      Range: File "", line 402, characters 6-35
                      Body Range: File "", line 402, characters 71-116
                      Content: |core: string -> michelson_program|
                      references: []
                      (create_chest#492 -> create_chest)
                      Range: File "", line 624, characters 6-18
                      Body Range: File "", line 624, characters 64-103
                      Content: |core: ( bytes * nat ) -> ( chest * chest_key )|
                      references: []
                      (create_chest_key#495 -> create_chest_key)
                      Range: File "", line 625, characters 6-22
                      Body Range: File "", line 625, characters 60-103
                      Content: |core: ( chest * nat ) -> chest_key|
                      references: []
                      (decompile#372 -> decompile)
                      Range: File "", line 395, characters 6-15
                      Body Range: File "", line 395, characters 16-24
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 413, characters 5-14
                      (drop_context#394 -> drop_context)
                      Range: File "", line 406, characters 6-18
                      Body Range: File "", line 406, characters 39-75
                      Content: |core: unit -> unit|
                      references: []
                      (eprint#352 -> eprint)
                      Range: File "", line 382, characters 6-12
                      Body Range: File "", line 382, characters 35-67
                      Content: |core: string -> unit|
                      references: []
                      (eval#334 -> eval)
                      Range: File "", line 369, characters 6-10
                      Body Range: File "", line 369, characters 11-19
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 374, characters 59-63 ,
                        File "", line 629, characters 34-38 ,
                        File "", line 634, characters 34-38 ,
                        File "", line 648, characters 12-16
                      (failwith#340 -> failwith)
                      Range: File "", line 376, characters 6-14
                      Body Range: File "", line 376, characters 15-25
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 725, characters 51-59 ,
                        File "", line 726, characters 74-82 ,
                        File "", line 727, characters 89-97 ,
                        File "", line 736, characters 69-77 ,
                        File "", line 737, characters 99-107 ,
                        File "", line 738, characters 114-122
                      (get_balance#348 -> get_balance)
                      Range: File "", line 380, characters 6-17
                      Body Range: File "", line 380, characters 40-75
                      Content: |core: address -> tez|
                      references: []
                      (get_bootstrap_account#361 -> get_bootstrap_account)
                      Range: File "", line 388, characters 6-27
                      Body Range: File "", line 388, characters 65-105
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      (get_last_events_from#448 -> get_last_events_from)
                      Range: File "", line 601, characters 6-26
                      Body Range: File "", line 601, characters 27-39
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * .
                      ( typed_address (p , s) * string ) -> list (a)|
                      references: []
                      (get_storage#403 -> get_storage)
                      Range: File "", line 409, characters 6-17
                      Body Range: File "", line 409, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references: []
                      (get_storage_of_address#346 -> get_storage_of_address)
                      Range: File "", line 379, characters 6-28
                      Body Range: File "", line 379, characters 65-111
                      Content: |core: address -> michelson_program|
                      references: File "", line 412, characters 32-54
                      (get_time#376 -> get_time)
                      Range: File "", line 397, characters 6-14
                      Body Range: File "", line 397, characters 41-57
                      Content: |core: unit -> timestamp|
                      references: []
                      (get_total_voting_power#338 -> get_total_voting_power)
                      Range: File "", line 375, characters 6-28
                      Body Range: File "", line 375, characters 49-96
                      Content: |core: unit -> nat|
                      references: []
                      (get_voting_power#354 -> get_voting_power)
                      Range: File "", line 383, characters 6-22
                      Body Range: File "", line 383, characters 47-88
                      Content: |core: key_hash -> nat|
                      references: []
                      (last_originations#365 -> last_originations)
                      Range: File "", line 390, characters 6-23
                      Body Range: File "", line 390, characters 67-108
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      (log#460 -> log)
                      Range: File "", line 611, characters 6-9
                      Body Range: File "", line 611, characters 10-18
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 640, characters 25-28
                      (michelson_equal#512 -> michelson_equal)
                      Range: File "", line 636, characters 6-21
                      Body Range: File "", line 636, characters 82-89
                      Content: |core: ( michelson_program * michelson_program ) -> bool|
                      references: []
                      (mutate_value#474 -> mutate_value)
                      Range: File "", line 618, characters 6-18
                      Body Range: File "", line 618, characters 19-27
                      Content: |core: ∀ a : * . ( nat * a ) -> option (
                      ( a * mutation ))|
                      references:
                        File "", line 665, characters 23-35 ,
                        File "", line 677, characters 23-35
                      (mutation_test#558 -> mutation_test)
                      Range: File "", line 661, characters 6-19
                      Body Range: File "", line 661, characters 20-30
                      Content: |core: ∀ a : * . ∀ b : * . ( a * a -> b ) -> option (
                      ( b * mutation ))|
                      references: []
                      (mutation_test_all#574 -> mutation_test_all)
                      Range: File "", line 673, characters 6-23
                      Body Range: File "", line 673, characters 24-34
                      Content: |core: ∀ a : * . ∀ b : * . ( a * a -> b ) -> list (
                      ( b * mutation ))|
                      references: []
                      (new_account#370 -> new_account)
                      Range: File "", line 394, characters 6-17
                      Body Range: File "", line 394, characters 46-81
                      Content: |core: unit -> ( string * key )|
                      references: []
                      (nl#418 -> nl)
                      Range: File "", line 431, characters 6-8
                      Body Range: File "", line 431, characters 11-53
                      Content: |unresolved|
                      references: File "", line 433, characters 15-17
                      (nth_bootstrap_account#359 -> nth_bootstrap_account)
                      Range: File "", line 385, characters 6-27
                      Body Range: File "", line 386, character 4 to line 387, character 5
                      Content: |core: int -> address|
                      references: []
                      (nth_bootstrap_contract#356 -> nth_bootstrap_contract)
                      Range: File "", line 384, characters 6-28
                      Body Range: File "", line 384, characters 51-97
                      Content: |core: nat -> address|
                      references: []
                      (nth_bootstrap_typed_address#363 -> nth_bootstrap_typed_address)
                      Range: File "", line 389, characters 6-33
                      Body Range: File "", line 389, characters 34-44
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      (originate#529 -> originate)
                      Range: File "", line 646, characters 6-15
                      Body Range: File "", line 646, characters 16-26
                      Content: |core: ∀ p : * . ∀ s : * . ( ( p * s ) -> ( list (operation) * s ) * s * tez ) ->
                      ( typed_address (p , s) * michelson_contract * int )|
                      references: []
                      (originate_contract#520 -> originate_contract)
                      Range: File "", line 645, characters 6-24
                      Body Range: File "", line 645, characters 96-135
                      Content: |core: ( michelson_contract * michelson_program * tez ) -> address|
                      references:
                        File "", line 649, characters 12-30 ,
                        File "", line 658, characters 12-30 ,
                        File "", line 688, characters 14-32 ,
                        File "", line 707, characters 14-32
                      (originate_from_file#543 -> originate_from_file)
                      Range: File "", line 656, characters 6-25
                      Body Range: File "", line 657, character 4 to line 660, character 13
                      Content: |core: ( string * string * list (string) * michelson_program * tez ) ->
                      ( address * michelson_contract * int )|
                      references: []
                      (originate_from_file_and_mutate#600 -> originate_from_file_and_mutate)
                      Range: File "", line 685, characters 6-36
                      Body Range: File "", line 685, characters 37-45
                      Content: |core: ∀ b : * . ( string * string * list (string) * michelson_program * tez * ( address * michelson_contract * int ) -> b ) -> option (
                      ( b * mutation ))|
                      references: []
                      (originate_from_file_and_mutate_all#627 -> originate_from_file_and_mutate_all)
                      Range: File "", line 704, characters 6-40
                      Body Range: File "", line 704, characters 41-49
                      Content: |core: ∀ b : * . ( string * string * list (string) * michelson_program * tez * ( address * michelson_contract * int ) -> b ) -> list (
                      ( b * mutation ))|
                      references: []
                      (parse_michelson#388 -> parse_michelson)
                      Range: File "", line 403, characters 6-21
                      Body Range: File "", line 403, characters 57-102
                      Content: |core: string -> michelson_program|
                      references: []
                      (print#350 -> print)
                      Range: File "", line 381, characters 6-11
                      Body Range: File "", line 381, characters 34-66
                      Content: |core: string -> unit|
                      references:
                        File "", line 433, characters 4-9 ,
                        File "", line 614, characters 4-9
                      (println#420 -> println)
                      Range: File "", line 432, characters 6-13
                      Body Range: File "", line 433, characters 4-18
                      Content: |core: string -> unit|
                      references: []
                      (random#368 -> random)
                      Range: File "", line 391, characters 6-12
                      Body Range: File "", line 391, characters 13-21
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      (read_contract_from_file#414 -> read_contract_from_file)
                      Range: File "", line 420, characters 6-29
                      Body Range: File "", line 420, characters 67-115
                      Content: |core: string -> michelson_contract|
                      references: []
                      (register_constant#382 -> register_constant)
                      Range: File "", line 400, characters 6-23
                      Body Range: File "", line 400, characters 59-100
                      Content: |core: michelson_program -> string|
                      references: []
                      (register_delegate#380 -> register_delegate)
                      Range: File "", line 399, characters 6-23
                      Body Range: File "", line 399, characters 49-91
                      Content: |core: key_hash -> unit|
                      references: []
                      (reset_state#463 -> reset_state)
                      Range: File "", line 615, characters 6-17
                      Body Range: File "", line 615, characters 53-118
                      Content: |core: ( nat * list (tez) ) -> unit|
                      references: []
                      (reset_state_at#467 -> reset_state_at)
                      Range: File "", line 616, characters 6-20
                      Body Range: File "", line 616, characters 71-119
                      Content: |core: ( timestamp * nat * list (tez) ) -> unit|
                      references: []
                      (restore_context#390 -> restore_context)
                      Range: File "", line 404, characters 6-21
                      Body Range: File "", line 404, characters 42-77
                      Content: |core: unit -> unit|
                      references: []
                      (run#331 -> run)
                      Range: File "", line 368, characters 6-9
                      Body Range: File "", line 368, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . ( a -> b * a ) -> michelson_program|
                      references: File "", line 369, characters 50-53
                      (save_context#392 -> save_context)
                      Range: File "", line 405, characters 6-18
                      Body Range: File "", line 405, characters 39-75
                      Content: |core: unit -> unit|
                      references: []
                      (save_mutation#477 -> save_mutation)
                      Range: File "", line 619, characters 6-19
                      Body Range: File "", line 619, characters 67-107
                      Content: |core: ( string * mutation ) -> option (string)|
                      references: []
                      (set_baker#407 -> set_baker)
                      Range: File "", line 415, characters 6-15
                      Body Range: File "", line 415, characters 39-70
                      Content: |core: address -> unit|
                      references: []
                      (set_baker_policy#405 -> set_baker_policy)
                      Range: File "", line 414, characters 6-22
                      Body Range: File "", line 414, characters 57-91
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 415, characters 39-55
                      (set_big_map#489 -> set_big_map)
                      Range: File "", line 623, characters 6-17
                      Body Range: File "", line 623, characters 18-28
                      Content: |core: ∀ a : * . ∀ b : * . ( int * big_map (a , b) ) -> unit|
                      references: []
                      (set_print_values#421 -> set_print_values)
                      Range: File "", line 435, characters 6-22
                      Body Range: File "", line 435, characters 43-100
                      Content: |core: unit -> unit|
                      references: []
                      (set_source#344 -> set_source)
                      Range: File "", line 378, characters 6-16
                      Body Range: File "", line 378, characters 40-74
                      Content: |core: address -> unit|
                      references: []
                      (sign#480 -> sign)
                      Range: File "", line 620, characters 6-10
                      Body Range: File "", line 620, characters 51-83
                      Content: |core: ( string * bytes ) -> signature|
                      references: []
                      (size#409 -> size)
                      Range: File "", line 416, characters 6-10
                      Body Range: File "", line 416, characters 44-72
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 650, characters 12-16 ,
                        File "", line 659, characters 12-16 ,
                        File "", line 689, characters 14-18 ,
                        File "", line 708, characters 14-18
                      (to_contract#342 -> to_contract)
                      Range: File "", line 377, characters 6-17
                      Body Range: File "", line 377, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 410, characters 25-36 ,
                        File "", line 602, characters 30-41
                      (to_entrypoint#516 -> to_entrypoint)
                      Range: File "", line 637, characters 6-19
                      Body Range: File "", line 637, characters 20-32
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * .
                      ( string * typed_address (a , b) ) -> contract (c)|
                      references: []
                      (to_json#398 -> to_json)
                      Range: File "", line 408, characters 6-13
                      Body Range: File "", line 408, characters 14-22
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      (to_string#396 -> to_string)
                      Range: File "", line 407, characters 6-15
                      Body Range: File "", line 407, characters 16-24
                      Content: |core: ∀ a : * . a -> string|
                      references:
                        File "", line 424, characters 68-77 ,
                        File "", line 426, characters 67-76 ,
                        File "", line 428, characters 61-70 ,
                        File "", line 613, characters 12-21
                      (to_typed_address#384 -> to_typed_address)
                      Range: File "", line 401, characters 6-22
                      Body Range: File "", line 401, characters 23-33
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      (transfer#452 -> transfer)
                      Range: File "", line 609, characters 6-14
                      Body Range: File "", line 609, characters 84-162
                      Content: |core: ( address * michelson_program * tez ) -> test_exec_result|
                      references: []
                      (transfer_exn#456 -> transfer_exn)
                      Range: File "", line 610, characters 6-18
                      Body Range: File "", line 610, characters 75-157
                      Content: |core: ( address * michelson_program * tez ) -> nat|
                      references: []
                      (transfer_to_contract#502 -> transfer_to_contract)
                      Range: File "", line 626, characters 6-26
                      Body Range: File "", line 626, characters 27-35
                      Content: |core: ∀ p : * . ( contract (p) * p * tez ) -> test_exec_result|
                      references: []
                      (transfer_to_contract_exn#509 -> transfer_to_contract_exn)
                      Range: File "", line 631, characters 6-30
                      Body Range: File "", line 631, characters 31-39
                      Content: |core: ∀ p : * . ( contract (p) * p * tez ) -> nat|
                      references: []
                      (unset_print_values#422 -> unset_print_values)
                      Range: File "", line 436, characters 6-24
                      Body Range: File "", line 436, characters 45-103
                      Content: |core: unit -> unit|
                      references: []
                      Type definitions:
                      Module definitions:
                      (PBT#438 -> PBT)
                      Range: File "", line 438, characters 9-12
                      Body Range: File "", line 438, character 2 to line 471, character 5
                      Content: Members: Variable definitions:
                                        (gen#423 -> gen)
                                        Range: File "", line 439, characters 8-11
                                        Body Range: File "", line 439, characters 12-20
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references:
                                          File "", line 457, characters 29-33 ,
                                          File "", line 458, characters 23-32 ,
                                          File "", line 458, characters 24-28 ,
                                          File "", line 459, characters 23-27
                                        (gen_small#424 -> gen_small)
                                        Range: File "", line 440, characters 8-17
                                        Body Range: File "", line 440, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (make_test#427 -> make_test)
                                        Range: File "", line 457, characters 8-17
                                        Body Range: File "", line 457, characters 18-26
                                        Content: |core: ∀ a : * . ( pbt_gen (a) * a -> bool ) -> pbt_test (a)|
                                        references: []
                                        (run#437 -> run)
                                        Range: File "", line 458, characters 8-11
                                        Body Range: File "", line 458, characters 12-20
                                        Content: |core: ∀ a : * . ( pbt_test (a) * nat ) -> pbt_result (a)|
                                        references: []
                                        Type definitions:
                                        Module definitions:

                      references: []


    references: []

    (Tezos#80 -> Tezos)
    Range: File "", line 8, characters 7-12
    Body Range: File "", line 8, character 0 to line 109, character 3
    Content: Members: Variable definitions:
                      (address#26 -> address)
                      Range: File "", line 21, characters 6-13
                      Body Range: File "", line 21, characters 14-22
                      Content: |core: ∀ a : * . contract (a) -> address|
                      references: File "", line 602, characters 21-28
                      (call_view#63 -> call_view)
                      Range: File "", line 91, characters 25-34
                      Body Range: File "", line 91, characters 35-45
                      Content: |core: ∀ a : * . ∀ b : * . ( string * a * address ) -> option (b)|
                      references: []
                      (constant#42 -> constant)
                      Range: File "", line 32, characters 25-33
                      Body Range: File "", line 32, characters 34-42
                      Content: |core: ∀ a : * . string -> a|
                      references: []
                      (create_contract#68 -> create_contract)
                      Range: File "", line 95, characters 25-40
                      Body Range: File "", line 95, characters 41-51
                      Content: |core: ∀ p : * . ∀ s : * . ( ( p * s ) -> ( list (operation) * s ) * option (key_hash) * tez * s ) ->
                      ( operation * address )|
                      references: []
                      (create_ticket#57 -> create_ticket)
                      Range: File "", line 83, characters 6-19
                      Body Range: File "", line 83, characters 20-28
                      Content: |core: ∀ a : * . ( a * nat ) -> option (ticket (a))|
                      references: []
                      (emit#76 -> emit)
                      Range: File "", line 103, characters 25-29
                      Body Range: File "", line 103, characters 30-38
                      Content: |core: ∀ a : * . ( string * a ) -> operation|
                      references: []
                      (get_amount#6 -> get_amount)
                      Range: File "", line 11, characters 6-16
                      Body Range: File "", line 11, characters 37-92
                      Content: |core: unit -> tez|
                      references: []
                      (get_balance#4 -> get_balance)
                      Range: File "", line 10, characters 6-17
                      Body Range: File "", line 10, characters 38-94
                      Content: |core: unit -> tez|
                      references: []
                      (get_chain_id#18 -> get_chain_id)
                      Range: File "", line 17, characters 6-18
                      Body Range: File "", line 17, characters 44-106
                      Content: |core: unit -> chain_id|
                      references: []
                      (get_contract#49 -> get_contract)
                      Range: File "", line 37, characters 25-37
                      Body Range: File "", line 37, characters 38-46
                      Content: |core: ∀ a : * . address -> contract (a)|
                      references: []
                      (get_contract_opt#45 -> get_contract_opt)
                      Range: File "", line 35, characters 25-41
                      Body Range: File "", line 35, characters 42-50
                      Content: |core: ∀ p : * . address -> option (contract (p))|
                      references:
                        File "", line 38, characters 12-28 ,
                        File "", line 77, characters 12-28
                      (get_contract_with_error#54 -> get_contract_with_error)
                      Range: File "", line 76, characters 6-29
                      Body Range: File "", line 76, characters 30-38
                      Content: |core: ∀ a : * . ( address * string ) -> contract (a)|
                      references: []
                      (get_entrypoint#74 -> get_entrypoint)
                      Range: File "", line 100, characters 25-39
                      Body Range: File "", line 100, characters 40-48
                      Content: |core: ∀ p : * . ( string * address ) -> contract (p)|
                      references: []
                      (get_entrypoint_opt#70 -> get_entrypoint_opt)
                      Range: File "", line 97, characters 25-43
                      Body Range: File "", line 97, characters 44-52
                      Content: |core: ∀ p : * . ( string * address ) -> option (contract (p))|
                      references: File "", line 101, characters 12-30
                      (get_level#14 -> get_level)
                      Range: File "", line 15, characters 6-15
                      Body Range: File "", line 15, characters 36-90
                      Content: |core: unit -> nat|
                      references: []
                      (get_min_block_time#22 -> get_min_block_time)
                      Range: File "", line 19, characters 6-24
                      Body Range: File "", line 19, characters 45-108
                      Content: |core: unit -> nat|
                      references: []
                      (get_now#8 -> get_now)
                      Range: File "", line 12, characters 6-13
                      Body Range: File "", line 12, characters 40-98
                      Content: |core: unit -> timestamp|
                      references: File "", line 397, characters 47-54
                      (get_self_address#16 -> get_self_address)
                      Range: File "", line 16, characters 6-22
                      Body Range: File "", line 16, characters 47-112
                      Content: |core: unit -> address|
                      references: []
                      (get_sender#10 -> get_sender)
                      Range: File "", line 13, characters 6-16
                      Body Range: File "", line 13, characters 41-100
                      Content: |core: unit -> address|
                      references: []
                      (get_source#12 -> get_source)
                      Range: File "", line 14, characters 6-16
                      Body Range: File "", line 14, characters 41-100
                      Content: |core: unit -> address|
                      references: []
                      (get_total_voting_power#20 -> get_total_voting_power)
                      Range: File "", line 18, characters 6-28
                      Body Range: File "", line 18, characters 49-116
                      Content: |core: unit -> nat|
                      references: []
                      (implicit_account#28 -> implicit_account)
                      Range: File "", line 22, characters 6-22
                      Body Range: File "", line 22, characters 57-129
                      Content: |core: key_hash -> contract (unit)|
                      references: []
                      (join_tickets#30 -> join_tickets)
                      Range: File "", line 23, characters 6-18
                      Body Range: File "", line 23, characters 19-27
                      Content: |core: ∀ a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a))|
                      references: []
                      (never#34 -> never)
                      Range: File "", line 26, characters 6-11
                      Body Range: File "", line 26, characters 12-20
                      Content: |core: ∀ a : * . never -> a|
                      references: []
                      (pairing_check#36 -> pairing_check)
                      Range: File "", line 27, characters 6-19
                      Body Range: File "", line 27, characters 70-155
                      Content: |core: list (( bls12_381_g1 * bls12_381_g2 )) -> bool|
                      references: []
                      (read_ticket#32 -> read_ticket)
                      Range: File "", line 24, characters 6-17
                      Body Range: File "", line 24, characters 18-26
                      Content: |core: ∀ a : * . ticket (a) -> ( ( address * ( a * nat ) ) * ticket (a) )|
                      references: []
                      (sapling_empty_state#43 -> sapling_empty_state)
                      Range: File "", line 33, characters 25-44
                      Body Range: File "", line 33, characters 45-57
                      Content: |core: ∀ sap_a : + . sapling_state (sap_a)|
                      references: []
                      (sapling_verify_update#79 -> sapling_verify_update)
                      Range: File "", line 106, characters 25-46
                      Body Range: File "", line 106, characters 47-59
                      Content: |core: ∀ sap_a : + . ( sapling_transaction (sap_a) * sapling_state (sap_a) ) -> option (
                      ( bytes * ( int * sapling_state (sap_a) ) ))|
                      references: []
                      (self#40 -> self)
                      Range: File "", line 29, characters 25-29
                      Body Range: File "", line 29, characters 30-38
                      Content: |core: ∀ a : * . string -> contract (a)|
                      references: []
                      (set_delegate#38 -> set_delegate)
                      Range: File "", line 28, characters 6-18
                      Body Range: File "", line 28, characters 55-125
                      Content: |core: option (key_hash) -> operation|
                      references: []
                      (split_ticket#66 -> split_ticket)
                      Range: File "", line 93, characters 6-18
                      Body Range: File "", line 93, characters 19-27
                      Content: |core: ∀ a : * . ( ticket (a) * ( nat * nat ) ) -> option (
                      ( ticket (a) * ticket (a) ))|
                      references: []
                      (transaction#61 -> transaction)
                      Range: File "", line 85, characters 6-17
                      Body Range: File "", line 85, characters 18-26
                      Content: |core: ∀ a : * . ( a * tez * contract (a) ) -> operation|
                      references: []
                      (voting_power#24 -> voting_power)
                      Range: File "", line 20, characters 6-18
                      Body Range: File "", line 20, characters 43-101
                      Content: |core: key_hash -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "", line 397, characters 41-46 ,
      File "", line 602, characters 15-20 |}]
