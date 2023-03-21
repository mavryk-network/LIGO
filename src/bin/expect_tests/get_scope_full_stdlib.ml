open Cli_expect

let gs s = "../../test/contracts/get_scope_tests/" ^ s
let () = Ligo_unix.putenv ~key:"LIGO_GET_SCOPE_USE_NEW_IMP" ~data:"true"

let%expect_test _ =
  run_ligo_good
    [ "info"; "get-scope"; gs "constant.mligo"; "--format"; "dev"; "--with-types" ];
  [%expect
    {|
    Scopes:
    [ Test#721 assert_none_with_error#720 assert_some_with_error#717 assert_with_error#714 assert_none#711 assert_some#709 assert#707 originate_from_file_and_mutate_all#705 originate_from_file_and_mutate#678 mutation_test_all#652 mutation_test#636 originate_from_file#621 compile_contract_from_file#612 originate_module#607 originate_uncurried#597 compile_contract_with_views#588 originate#584 originate_contract#575 to_entrypoint#571 michelson_equal#567 transfer_to_contract_exn#564 transfer_to_contract#557 create_chest_key#550 create_chest#547 set_big_map#544 baker_account#541 add_account#538 sign#535 save_mutation#532 mutate_value#529 bootstrap_contract#526 reset_state_at#522 reset_state#518 log#515 transfer_exn#511 transfer#507 get_last_events_from#503 PBT#494 run#493 make_test#485 gen_small#482 gen#481 unset_print_values#480 set_print_values#479 println#478 nl#476 chr#475 read_contract_from_file#472 compile_contract#470 size#466 set_baker#464 set_baker_policy#462 get_storage#460 to_json#455 to_string#453 drop_context#451 save_context#449 restore_context#447 parse_michelson#445 constant_to_michelson_program#443 to_typed_address#441 register_constant#439 register_delegate#437 cast_address#435 get_time#433 bake_until_n_cycle_end#431 decompile#429 new_account#427 random#425 last_originations#422 nth_bootstrap_typed_address#420 get_bootstrap_account#418 nth_bootstrap_account#416 nth_bootstrap_contract#413 get_voting_power#411 eprint#409 print#407 get_balance#405 get_storage_of_address#403 set_source#401 to_contract#399 failwith#397 get_total_voting_power#395 compile_value#393 eval#391 run#388 unforged_ticket#385 pbt_result#384 pbt_test#383 test_baker_policy#382 test_exec_result#381 test_exec_error#380 test_exec_error_balance_too_low#379 ediv#378 assert_none_with_error#375 assert_some_with_error#372 assert_with_error#369 uncurry#366 curry#363 ignore#359 int#358 unit#356 false#355 true#354 is_nat#353 abs#351 assert_none#349 assert_some#347 assert#345 Crypto#343 check#342 hash_key#338 keccak#336 sha3#334 sha512#332 sha256#330 blake2b#328 Bytes#326 sub#325 concat#321 length#318 unpack#316 pack#314 concats#312 Option#310 is_some#309 is_none#307 value_exn#305 value#301 map#297 unopt_with_error#294 unopt#290 String#287 sub#286 concat#282 concats#279 length#277 List#275 update_with#274 update#269 filter_map#264 find_opt#259 cons#255 fold_right#252 fold_left#248 fold#244 iter#240 map#237 tail_opt#234 head_opt#231 size#228 length#226 Set#224 fold_desc#223 fold#219 iter#215 update#212 remove#208 add#205 mem#202 literal#199 cardinal#197 size#195 empty#193 Transpiled#192 map_remove#191 map_add#188 map_find_opt#184 Map#181 fold#180 map#176 iter#173 find_opt#170 find#167 get_and_update#164 update#160 remove#156 add#153 mem#149 literal#146 size#144 empty#142 Big_map#141 find#140 find_opt#137 get_and_update#134 update#130 remove#126 add#123 mem#119 literal#116 empty#114 Bitwise#113 shift_right#112 shift_left#109 or#106 xor#103 and#100 Tezos#97 sapling_verify_update#96 emit#93 get_entrypoint#90 get_entrypoint_opt#85 create_contract_uncurried#82 create_contract#77 split_ticket#69 call_view#66 transaction#62 create_ticket#58 get_contract_with_error#55 get_contract#50 get_contract_opt#46 sapling_empty_state#44 constant#43 self#41 set_delegate#39 pairing_check#37 never#35 read_ticket#33 join_tickets#31 implicit_account#29 address#27 voting_power#25 get_min_block_time#23 get_total_voting_power#21 get_chain_id#19 get_self_address#17 get_level#15 get_source#13 get_sender#11 get_now#9 get_amount#7 get_balance#5 option#3 bool#2 failwith#1  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    [ c#723 a#722 Test#721 assert_none_with_error#720 assert_some_with_error#717 assert_with_error#714 assert_none#711 assert_some#709 assert#707 originate_from_file_and_mutate_all#705 originate_from_file_and_mutate#678 mutation_test_all#652 mutation_test#636 originate_from_file#621 compile_contract_from_file#612 originate_module#607 originate_uncurried#597 compile_contract_with_views#588 originate#584 originate_contract#575 to_entrypoint#571 michelson_equal#567 transfer_to_contract_exn#564 transfer_to_contract#557 create_chest_key#550 create_chest#547 set_big_map#544 baker_account#541 add_account#538 sign#535 save_mutation#532 mutate_value#529 bootstrap_contract#526 reset_state_at#522 reset_state#518 log#515 transfer_exn#511 transfer#507 get_last_events_from#503 PBT#494 run#493 make_test#485 gen_small#482 gen#481 unset_print_values#480 set_print_values#479 println#478 nl#476 chr#475 read_contract_from_file#472 compile_contract#470 size#466 set_baker#464 set_baker_policy#462 get_storage#460 to_json#455 to_string#453 drop_context#451 save_context#449 restore_context#447 parse_michelson#445 constant_to_michelson_program#443 to_typed_address#441 register_constant#439 register_delegate#437 cast_address#435 get_time#433 bake_until_n_cycle_end#431 decompile#429 new_account#427 random#425 last_originations#422 nth_bootstrap_typed_address#420 get_bootstrap_account#418 nth_bootstrap_account#416 nth_bootstrap_contract#413 get_voting_power#411 eprint#409 print#407 get_balance#405 get_storage_of_address#403 set_source#401 to_contract#399 failwith#397 get_total_voting_power#395 compile_value#393 eval#391 run#388 unforged_ticket#385 pbt_result#384 pbt_test#383 test_baker_policy#382 test_exec_result#381 test_exec_error#380 test_exec_error_balance_too_low#379 ediv#378 assert_none_with_error#375 assert_some_with_error#372 assert_with_error#369 uncurry#366 curry#363 ignore#359 int#358 unit#356 false#355 true#354 is_nat#353 abs#351 assert_none#349 assert_some#347 assert#345 Crypto#343 check#342 hash_key#338 keccak#336 sha3#334 sha512#332 sha256#330 blake2b#328 Bytes#326 sub#325 concat#321 length#318 unpack#316 pack#314 concats#312 Option#310 is_some#309 is_none#307 value_exn#305 value#301 map#297 unopt_with_error#294 unopt#290 String#287 sub#286 concat#282 concats#279 length#277 List#275 update_with#274 update#269 filter_map#264 find_opt#259 cons#255 fold_right#252 fold_left#248 fold#244 iter#240 map#237 tail_opt#234 head_opt#231 size#228 length#226 Set#224 fold_desc#223 fold#219 iter#215 update#212 remove#208 add#205 mem#202 literal#199 cardinal#197 size#195 empty#193 Transpiled#192 map_remove#191 map_add#188 map_find_opt#184 Map#181 fold#180 map#176 iter#173 find_opt#170 find#167 get_and_update#164 update#160 remove#156 add#153 mem#149 literal#146 size#144 empty#142 Big_map#141 find#140 find_opt#137 get_and_update#134 update#130 remove#126 add#123 mem#119 literal#116 empty#114 Bitwise#113 shift_right#112 shift_left#109 or#106 xor#103 and#100 Tezos#97 sapling_verify_update#96 emit#93 get_entrypoint#90 get_entrypoint_opt#85 create_contract_uncurried#82 create_contract#77 split_ticket#69 call_view#66 transaction#62 create_ticket#58 get_contract_with_error#55 get_contract#50 get_contract_opt#46 sapling_empty_state#44 constant#43 self#41 set_delegate#39 pairing_check#37 never#35 read_ticket#33 join_tickets#31 implicit_account#29 address#27 voting_power#25 get_min_block_time#23 get_total_voting_power#21 get_chain_id#19 get_self_address#17 get_level#15 get_source#13 get_sender#11 get_now#9 get_amount#7 get_balance#5 option#3 bool#2 failwith#1  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    [ d#724 c#723 a#722 Test#721 assert_none_with_error#720 assert_some_with_error#717 assert_with_error#714 assert_none#711 assert_some#709 assert#707 originate_from_file_and_mutate_all#705 originate_from_file_and_mutate#678 mutation_test_all#652 mutation_test#636 originate_from_file#621 compile_contract_from_file#612 originate_module#607 originate_uncurried#597 compile_contract_with_views#588 originate#584 originate_contract#575 to_entrypoint#571 michelson_equal#567 transfer_to_contract_exn#564 transfer_to_contract#557 create_chest_key#550 create_chest#547 set_big_map#544 baker_account#541 add_account#538 sign#535 save_mutation#532 mutate_value#529 bootstrap_contract#526 reset_state_at#522 reset_state#518 log#515 transfer_exn#511 transfer#507 get_last_events_from#503 PBT#494 run#493 make_test#485 gen_small#482 gen#481 unset_print_values#480 set_print_values#479 println#478 nl#476 chr#475 read_contract_from_file#472 compile_contract#470 size#466 set_baker#464 set_baker_policy#462 get_storage#460 to_json#455 to_string#453 drop_context#451 save_context#449 restore_context#447 parse_michelson#445 constant_to_michelson_program#443 to_typed_address#441 register_constant#439 register_delegate#437 cast_address#435 get_time#433 bake_until_n_cycle_end#431 decompile#429 new_account#427 random#425 last_originations#422 nth_bootstrap_typed_address#420 get_bootstrap_account#418 nth_bootstrap_account#416 nth_bootstrap_contract#413 get_voting_power#411 eprint#409 print#407 get_balance#405 get_storage_of_address#403 set_source#401 to_contract#399 failwith#397 get_total_voting_power#395 compile_value#393 eval#391 run#388 unforged_ticket#385 pbt_result#384 pbt_test#383 test_baker_policy#382 test_exec_result#381 test_exec_error#380 test_exec_error_balance_too_low#379 ediv#378 assert_none_with_error#375 assert_some_with_error#372 assert_with_error#369 uncurry#366 curry#363 ignore#359 int#358 unit#356 false#355 true#354 is_nat#353 abs#351 assert_none#349 assert_some#347 assert#345 Crypto#343 check#342 hash_key#338 keccak#336 sha3#334 sha512#332 sha256#330 blake2b#328 Bytes#326 sub#325 concat#321 length#318 unpack#316 pack#314 concats#312 Option#310 is_some#309 is_none#307 value_exn#305 value#301 map#297 unopt_with_error#294 unopt#290 String#287 sub#286 concat#282 concats#279 length#277 List#275 update_with#274 update#269 filter_map#264 find_opt#259 cons#255 fold_right#252 fold_left#248 fold#244 iter#240 map#237 tail_opt#234 head_opt#231 size#228 length#226 Set#224 fold_desc#223 fold#219 iter#215 update#212 remove#208 add#205 mem#202 literal#199 cardinal#197 size#195 empty#193 Transpiled#192 map_remove#191 map_add#188 map_find_opt#184 Map#181 fold#180 map#176 iter#173 find_opt#170 find#167 get_and_update#164 update#160 remove#156 add#153 mem#149 literal#146 size#144 empty#142 Big_map#141 find#140 find_opt#137 get_and_update#134 update#130 remove#126 add#123 mem#119 literal#116 empty#114 Bitwise#113 shift_right#112 shift_left#109 or#106 xor#103 and#100 Tezos#97 sapling_verify_update#96 emit#93 get_entrypoint#90 get_entrypoint_opt#85 create_contract_uncurried#82 create_contract#77 split_ticket#69 call_view#66 transaction#62 create_ticket#58 get_contract_with_error#55 get_contract#50 get_contract_opt#46 sapling_empty_state#44 constant#43 self#41 set_delegate#39 pairing_check#37 never#35 read_ticket#33 join_tickets#31 implicit_account#29 address#27 voting_power#25 get_min_block_time#23 get_total_voting_power#21 get_chain_id#19 get_self_address#17 get_level#15 get_source#13 get_sender#11 get_now#9 get_amount#7 get_balance#5 option#3 bool#2 failwith#1  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-44
    [ e#725 a#722 Test#721 assert_none_with_error#720 assert_some_with_error#717 assert_with_error#714 assert_none#711 assert_some#709 assert#707 originate_from_file_and_mutate_all#705 originate_from_file_and_mutate#678 mutation_test_all#652 mutation_test#636 originate_from_file#621 compile_contract_from_file#612 originate_module#607 originate_uncurried#597 compile_contract_with_views#588 originate#584 originate_contract#575 to_entrypoint#571 michelson_equal#567 transfer_to_contract_exn#564 transfer_to_contract#557 create_chest_key#550 create_chest#547 set_big_map#544 baker_account#541 add_account#538 sign#535 save_mutation#532 mutate_value#529 bootstrap_contract#526 reset_state_at#522 reset_state#518 log#515 transfer_exn#511 transfer#507 get_last_events_from#503 PBT#494 run#493 make_test#485 gen_small#482 gen#481 unset_print_values#480 set_print_values#479 println#478 nl#476 chr#475 read_contract_from_file#472 compile_contract#470 size#466 set_baker#464 set_baker_policy#462 get_storage#460 to_json#455 to_string#453 drop_context#451 save_context#449 restore_context#447 parse_michelson#445 constant_to_michelson_program#443 to_typed_address#441 register_constant#439 register_delegate#437 cast_address#435 get_time#433 bake_until_n_cycle_end#431 decompile#429 new_account#427 random#425 last_originations#422 nth_bootstrap_typed_address#420 get_bootstrap_account#418 nth_bootstrap_account#416 nth_bootstrap_contract#413 get_voting_power#411 eprint#409 print#407 get_balance#405 get_storage_of_address#403 set_source#401 to_contract#399 failwith#397 get_total_voting_power#395 compile_value#393 eval#391 run#388 unforged_ticket#385 pbt_result#384 pbt_test#383 test_baker_policy#382 test_exec_result#381 test_exec_error#380 test_exec_error_balance_too_low#379 ediv#378 assert_none_with_error#375 assert_some_with_error#372 assert_with_error#369 uncurry#366 curry#363 ignore#359 int#358 unit#356 false#355 true#354 is_nat#353 abs#351 assert_none#349 assert_some#347 assert#345 Crypto#343 check#342 hash_key#338 keccak#336 sha3#334 sha512#332 sha256#330 blake2b#328 Bytes#326 sub#325 concat#321 length#318 unpack#316 pack#314 concats#312 Option#310 is_some#309 is_none#307 value_exn#305 value#301 map#297 unopt_with_error#294 unopt#290 String#287 sub#286 concat#282 concats#279 length#277 List#275 update_with#274 update#269 filter_map#264 find_opt#259 cons#255 fold_right#252 fold_left#248 fold#244 iter#240 map#237 tail_opt#234 head_opt#231 size#228 length#226 Set#224 fold_desc#223 fold#219 iter#215 update#212 remove#208 add#205 mem#202 literal#199 cardinal#197 size#195 empty#193 Transpiled#192 map_remove#191 map_add#188 map_find_opt#184 Map#181 fold#180 map#176 iter#173 find_opt#170 find#167 get_and_update#164 update#160 remove#156 add#153 mem#149 literal#146 size#144 empty#142 Big_map#141 find#140 find_opt#137 get_and_update#134 update#130 remove#126 add#123 mem#119 literal#116 empty#114 Bitwise#113 shift_right#112 shift_left#109 or#106 xor#103 and#100 Tezos#97 sapling_verify_update#96 emit#93 get_entrypoint#90 get_entrypoint_opt#85 create_contract_uncurried#82 create_contract#77 split_ticket#69 call_view#66 transaction#62 create_ticket#58 get_contract_with_error#55 get_contract#50 get_contract_opt#46 sapling_empty_state#44 constant#43 self#41 set_delegate#39 pairing_check#37 never#35 read_ticket#33 join_tickets#31 implicit_account#29 address#27 voting_power#25 get_min_block_time#23 get_total_voting_power#21 get_chain_id#19 get_self_address#17 get_level#15 get_source#13 get_sender#11 get_now#9 get_amount#7 get_balance#5 option#3 bool#2 failwith#1  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 18-32
    [ a#722 Test#721 assert_none_with_error#720 assert_some_with_error#717 assert_with_error#714 assert_none#711 assert_some#709 assert#707 originate_from_file_and_mutate_all#705 originate_from_file_and_mutate#678 mutation_test_all#652 mutation_test#636 originate_from_file#621 compile_contract_from_file#612 originate_module#607 originate_uncurried#597 compile_contract_with_views#588 originate#584 originate_contract#575 to_entrypoint#571 michelson_equal#567 transfer_to_contract_exn#564 transfer_to_contract#557 create_chest_key#550 create_chest#547 set_big_map#544 baker_account#541 add_account#538 sign#535 save_mutation#532 mutate_value#529 bootstrap_contract#526 reset_state_at#522 reset_state#518 log#515 transfer_exn#511 transfer#507 get_last_events_from#503 PBT#494 run#493 make_test#485 gen_small#482 gen#481 unset_print_values#480 set_print_values#479 println#478 nl#476 chr#475 read_contract_from_file#472 compile_contract#470 size#466 set_baker#464 set_baker_policy#462 get_storage#460 to_json#455 to_string#453 drop_context#451 save_context#449 restore_context#447 parse_michelson#445 constant_to_michelson_program#443 to_typed_address#441 register_constant#439 register_delegate#437 cast_address#435 get_time#433 bake_until_n_cycle_end#431 decompile#429 new_account#427 random#425 last_originations#422 nth_bootstrap_typed_address#420 get_bootstrap_account#418 nth_bootstrap_account#416 nth_bootstrap_contract#413 get_voting_power#411 eprint#409 print#407 get_balance#405 get_storage_of_address#403 set_source#401 to_contract#399 failwith#397 get_total_voting_power#395 compile_value#393 eval#391 run#388 unforged_ticket#385 pbt_result#384 pbt_test#383 test_baker_policy#382 test_exec_result#381 test_exec_error#380 test_exec_error_balance_too_low#379 ediv#378 assert_none_with_error#375 assert_some_with_error#372 assert_with_error#369 uncurry#366 curry#363 ignore#359 int#358 unit#356 false#355 true#354 is_nat#353 abs#351 assert_none#349 assert_some#347 assert#345 Crypto#343 check#342 hash_key#338 keccak#336 sha3#334 sha512#332 sha256#330 blake2b#328 Bytes#326 sub#325 concat#321 length#318 unpack#316 pack#314 concats#312 Option#310 is_some#309 is_none#307 value_exn#305 value#301 map#297 unopt_with_error#294 unopt#290 String#287 sub#286 concat#282 concats#279 length#277 List#275 update_with#274 update#269 filter_map#264 find_opt#259 cons#255 fold_right#252 fold_left#248 fold#244 iter#240 map#237 tail_opt#234 head_opt#231 size#228 length#226 Set#224 fold_desc#223 fold#219 iter#215 update#212 remove#208 add#205 mem#202 literal#199 cardinal#197 size#195 empty#193 Transpiled#192 map_remove#191 map_add#188 map_find_opt#184 Map#181 fold#180 map#176 iter#173 find_opt#170 find#167 get_and_update#164 update#160 remove#156 add#153 mem#149 literal#146 size#144 empty#142 Big_map#141 find#140 find_opt#137 get_and_update#134 update#130 remove#126 add#123 mem#119 literal#116 empty#114 Bitwise#113 shift_right#112 shift_left#109 or#106 xor#103 and#100 Tezos#97 sapling_verify_update#96 emit#93 get_entrypoint#90 get_entrypoint_opt#85 create_contract_uncurried#82 create_contract#77 split_ticket#69 call_view#66 transaction#62 create_ticket#58 get_contract_with_error#55 get_contract#50 get_contract_opt#46 sapling_empty_state#44 constant#43 self#41 set_delegate#39 pairing_check#37 never#35 read_ticket#33 join_tickets#31 implicit_account#29 address#27 voting_power#25 get_min_block_time#23 get_total_voting_power#21 get_chain_id#19 get_self_address#17 get_level#15 get_source#13 get_sender#11 get_now#9 get_amount#7 get_balance#5 option#3 bool#2 failwith#1  ] File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33

    Variable definitions:
    (a#722 -> a)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 1, characters 8-9
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 43-44 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 22-23 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 29-30
    (abs#351 -> abs)
    Range: File "", line 202, characters 4-7
    Body Range: File "", line 202, characters 9-10
    Content: |core: int -> nat|
    references: File "", line 366, characters 31-34
    (assert#345 -> assert)
    Range: File "", line 199, characters 4-10
    Body Range: File "", line 199, characters 12-13
    Content: |core: bool -> unit|
    references: []
    (assert_none#349 -> assert_none)
    Range: File "", line 201, characters 4-15
    Body Range: File "", line 201, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_none_with_error#375 -> assert_none_with_error)
    Range: File "", line 214, characters 4-26
    Body Range: File "", line 214, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_some#347 -> assert_some)
    Range: File "", line 200, characters 4-15
    Body Range: File "", line 200, characters 16-24
    Content: |core: ∀ a : * . option (a) -> unit|
    references: []
    (assert_some_with_error#372 -> assert_some_with_error)
    Range: File "", line 213, characters 4-26
    Body Range: File "", line 213, characters 27-35
    Content: |core: ∀ a : * . option (a) -> string -> unit|
    references: []
    (assert_with_error#369 -> assert_with_error)
    Range: File "", line 212, characters 4-21
    Body Range: File "", line 212, characters 23-24
    Content: |unresolved|
    references: []
    (b#726 -> b)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 3, characters 4-5
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 4, character 2 to line 6, character 33
    Content: |resolved: list (int)|
    references: []
    (c#723 -> c)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 10-11
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 22-44
    Content: |core: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 39-40
    (curry#363 -> curry)
    Range: File "", line 209, characters 4-9
    Body Range: File "", line 209, characters 10-22
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . ( a * b ) -> c -> a -> b -> c|
    references: []
    (d#724 -> d)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 26-27
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 30-31
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 5, characters 35-36
    (e#725 -> e)
    Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 9-10
    Body Range: File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 13-14
    Content: |resolved: int|
    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 20-21 ,
      File "../../test/contracts/get_scope_tests/constant.mligo", line 6, characters 27-28
    (ediv#378 -> ediv)
    Range: File "", line 215, characters 4-8
    Body Range: File "", line 215, characters 9-19
    Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_ediv (a ,
    b)|
    references: []
    (failwith#1 -> failwith)
    Range: File "", line 2, characters 4-12
    Body Range: File "", line 2, characters 13-23
    Content: |unresolved|
    references:
      File "", line 38, characters 27-35 ,
      File "", line 42, characters 27-35 ,
      File "", line 69, characters 27-35 ,
      File "", line 169, characters 79-87 ,
      File "", line 171, characters 103-111 ,
      File "", line 174, characters 83-91 ,
      File "", line 199, characters 49-57 ,
      File "", line 200, characters 72-80 ,
      File "", line 201, characters 87-95 ,
      File "", line 212, characters 66-74 ,
      File "", line 213, characters 96-104 ,
      File "", line 214, characters 111-119
    (false#355 -> false)
    Range: File "", line 205, characters 4-9
    Body Range: File "", line 205, characters 19-24
    Content: |core: bool|
    references:
      File "", line 261, characters 51-56 ,
      File "", line 306, characters 90-95 ,
      File "", line 309, characters 62-67
    (ignore#359 -> ignore)
    Range: File "", line 208, characters 4-10
    Body Range: File "", line 208, characters 11-19
    Content: |core: ∀ a : * . a -> unit|
    references: []
    (int#358 -> int)
    Range: File "", line 207, characters 4-7
    Body Range: File "", line 207, characters 8-16
    Content: |core: ∀ a : * . a -> external_int (a)|
    references:
      File "", line 257, characters 97-100 ,
      File "", line 294, characters 79-82 ,
      File "", line 296, characters 78-81 ,
      File "", line 298, characters 72-75
    (is_nat#353 -> is_nat)
    Range: File "", line 203, characters 4-10
    Body Range: File "", line 203, characters 12-13
    Content: |core: int -> option (nat)|
    references: []
    (true#354 -> true)
    Range: File "", line 204, characters 4-8
    Body Range: File "", line 204, characters 18-22
    Content: |core: bool|
    references:
      File "", line 305, characters 88-92 ,
      File "", line 310, characters 68-72
    (uncurry#366 -> uncurry)
    Range: File "", line 210, characters 4-11
    Body Range: File "", line 210, characters 12-24
    Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . a -> b -> c -> ( a * b ) -> c|
    references: File "", line 372, characters 30-37
    (unit#356 -> unit)
    Range: File "", line 206, characters 4-8
    Body Range: File "", line 206, characters 18-38
    Content: |core: unit|
    references: []
    Type definitions:
    (bool#2 -> bool)
    Range: File "", line 4, characters 5-9
    Body Range: File "", line 4, characters 12-24
    Content: : |sum[False -> unit , True -> unit]|
    references:
      File "", line 26, characters 63-67 ,
      File "", line 26, characters 111-115 ,
      File "", line 89, characters 52-56 ,
      File "", line 104, characters 48-52 ,
      File "", line 129, characters 41-45 ,
      File "", line 132, characters 35-39 ,
      File "", line 150, characters 34-38 ,
      File "", line 156, characters 37-41 ,
      File "", line 175, characters 40-44 ,
      File "", line 176, characters 40-44 ,
      File "", line 196, characters 52-56 ,
      File "", line 196, characters 106-110 ,
      File "", line 199, characters 16-20 ,
      File "", line 204, characters 11-15 ,
      File "", line 205, characters 12-16 ,
      File "", line 212, characters 27-31 ,
      File "", line 233, characters 41-45 ,
      File "", line 311, characters 53-57 ,
      File "", line 361, characters 74-78 ,
      File "", line 468, characters 18-22 ,
      File "", line 472, characters 29-33
    (option#3 -> option)
    Range: File "", line 5, characters 8-14
    Body Range: File "", line 5, characters 0-34
    Content: : |funtype 'a : * . option ('a)|
    references:
      File "", line 22, characters 56-73 ,
      File "", line 22, characters 116-131 ,
      File "", line 27, characters 24-39 ,
      File "", line 29, characters 12-20 ,
      File "", line 33, characters 68-80 ,
      File "", line 34, characters 67-86 ,
      File "", line 35, characters 57-65 ,
      File "", line 35, characters 71-90 ,
      File "", line 47, characters 49-66 ,
      File "", line 47, characters 105-122 ,
      File "", line 55, characters 84-92 ,
      File "", line 56, characters 78-86 ,
      File "", line 56, characters 94-102 ,
      File "", line 57, characters 61-89 ,
      File "", line 58, characters 46-74 ,
      File "", line 59, characters 93-108 ,
      File "", line 62, characters 102-117 ,
      File "", line 64, characters 82-99 ,
      File "", line 66, characters 81-89 ,
      File "", line 66, characters 95-114 ,
      File "", line 72, characters 77-85 ,
      File "", line 73, characters 120-164 ,
      File "", line 73, characters 218-262 ,
      File "", line 92, characters 37-45 ,
      File "", line 93, characters 45-53 ,
      File "", line 93, characters 78-86 ,
      File "", line 94, characters 57-65 ,
      File "", line 107, characters 37-45 ,
      File "", line 108, characters 45-53 ,
      File "", line 108, characters 74-82 ,
      File "", line 110, characters 53-61 ,
      File "", line 120, characters 141-182 ,
      File "", line 141, characters 40-48 ,
      File "", line 142, characters 40-55 ,
      File "", line 150, characters 56-64 ,
      File "", line 151, characters 29-37 ,
      File "", line 152, characters 38-46 ,
      File "", line 154, characters 32-40 ,
      File "", line 169, characters 26-34 ,
      File "", line 171, characters 37-45 ,
      File "", line 172, characters 48-56 ,
      File "", line 172, characters 60-68 ,
      File "", line 173, characters 40-48 ,
      File "", line 174, characters 46-54 ,
      File "", line 175, characters 28-36 ,
      File "", line 176, characters 28-36 ,
      File "", line 182, characters 36-44 ,
      File "", line 182, characters 98-106 ,
      File "", line 182, characters 112-120 ,
      File "", line 200, characters 30-38 ,
      File "", line 201, characters 30-38 ,
      File "", line 203, characters 23-33 ,
      File "", line 203, characters 69-79 ,
      File "", line 213, characters 41-49 ,
      File "", line 214, characters 41-49 ,
      File "", line 291, characters 22-35 ,
      File "", line 334, characters 140-153 ,
      File "", line 335, characters 135-148 ,
      File "", line 340, characters 92-108 ,
      File "", line 343, characters 48-69 ,
      File "", line 344, characters 50-63 ,
      File "", line 347, characters 44-54 ,
      File "", line 353, characters 12-25 ,
      File "", line 358, characters 14-27 ,
      File "", line 396, characters 96-106 ,
      File "", line 403, characters 59-80 ,
      File "", line 406, characters 37-58 ,
      File "", line 428, characters 90-111 ,
      File "", line 434, characters 96-106 ,
      File "", line 437, characters 37-58 ,
      File "", line 454, characters 96-106 ,
      File "", line 469, characters 32-40 ,
      File "", line 470, characters 32-40 ,
      File "", line 473, characters 43-51 ,
      File "", line 474, characters 43-51
    (pbt_result#384 -> pbt_result)
    Range: File "", line 234, characters 8-18
    Body Range: File "", line 234, characters 0-41
    Content: : |funtype 'a : * . sum[Fail -> 'a , Success -> unit]|
    references:
      File "", line 312, characters 55-67 ,
      File "", line 313, characters 37-49 ,
      File "", line 315, characters 82-94 ,
      File "", line 319, characters 94-106 ,
      File "", line 322, characters 66-78
    (pbt_test#383 -> pbt_test)
    Range: File "", line 233, characters 8-16
    Body Range: File "", line 233, characters 0-46
    Content: : |funtype 'a : * . ( pbt_gen ('a) * 'a -> bool )|
    references:
      File "", line 311, characters 61-71 ,
      File "", line 312, characters 31-41
    (test_baker_policy#382 -> test_baker_policy)
    Range: File "", line 228, characters 5-22
    Body Range: File "", line 229, character 4 to line 231, character 29
    Content: : |sum[By_account -> address ,
                    By_round -> int ,
                    Excluding -> list (address)]|
    references: File "", line 283, characters 29-46
    (test_exec_error#380 -> test_exec_error)
    Range: File "", line 221, characters 5-20
    Body Range: File "", line 222, character 4 to line 224, character 19
    Content: : |sum[Balance_too_low -> test_exec_error_balance_too_low ,
                    Other -> string ,
                    Rejected -> ( michelson_program * address )]|
    references: File "", line 226, characters 49-64
    (test_exec_error_balance_too_low#379 -> test_exec_error_balance_too_low)
    Range: File "", line 218, characters 5-36
    Body Range: File "", line 219, characters 2-79
    Content: : |record[contract_balance -> tez ,
                       contract_too_low -> address ,
                       spend_request -> tez]|
    references: File "", line 223, characters 23-54
    (test_exec_result#381 -> test_exec_result)
    Range: File "", line 226, characters 5-21
    Body Range: File "", line 226, characters 24-64
    Content: : |sum[Fail -> test_exec_error , Success -> nat]|
    references:
      File "", line 334, characters 65-81 ,
      File "", line 351, characters 73-89
    (unforged_ticket#385 -> unforged_ticket)
    Range: File "", line 236, characters 8-23
    Body Range: File "", line 236, characters 0-91
    Content: : |funtype 's : * . record[amount -> nat ,
                                        ticketer -> address ,
                                        value -> 's({ name: ticketer }, { name: value }, { name: amount })]|
    references: []
    Module definitions:
    (Big_map#141 -> Big_map)
    Range: File "", line 85, characters 7-14
    Body Range: File "", line 85, character 0 to line 97, character 3
    Content: Members: Variable definitions:
                      (add#123 -> add)
                      Range: File "", line 90, characters 6-9
                      Body Range: File "", line 90, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (empty#114 -> empty)
                      Range: File "", line 86, characters 16-21
                      Body Range: File "", line 86, characters 22-32
                      Content: |core: ∀ k : * . ∀ v : * . big_map (k ,
                      v)|
                      references: []
                      (find#140 -> find)
                      Range: File "", line 95, characters 6-10
                      Body Range: File "", line 95, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> v|
                      references: []
                      (find_opt#137 -> find_opt)
                      Range: File "", line 94, characters 6-14
                      Body Range: File "", line 94, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> option (v)|
                      references: []
                      (get_and_update#134 -> get_and_update)
                      Range: File "", line 93, characters 6-20
                      Body Range: File "", line 93, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> ( option (v) * big_map (k , v) )|
                      references: []
                      (literal#116 -> literal)
                      Range: File "", line 87, characters 25-32
                      Body Range: File "", line 87, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> big_map (k ,
                      v)|
                      references: []
                      (mem#119 -> mem)
                      Range: File "", line 89, characters 6-9
                      Body Range: File "", line 89, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> bool|
                      references: []
                      (remove#126 -> remove)
                      Range: File "", line 91, characters 6-12
                      Body Range: File "", line 91, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      (update#130 -> update)
                      Range: File "", line 92, characters 6-12
                      Body Range: File "", line 92, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> big_map (k ,
                      v) -> big_map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bitwise#113 -> Bitwise)
    Range: File "", line 77, characters 7-14
    Body Range: File "", line 77, character 0 to line 83, character 3
    Content: Members: Variable definitions:
                      (and#100 -> and)
                      Range: File "", line 78, characters 6-10
                      Body Range: File "", line 78, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> external_and (a ,
                      b)|
                      references: []
                      (or#106 -> or)
                      Range: File "", line 80, characters 6-9
                      Body Range: File "", line 80, characters 11-12
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_left#109 -> shift_left)
                      Range: File "", line 81, characters 6-16
                      Body Range: File "", line 81, characters 18-19
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (shift_right#112 -> shift_right)
                      Range: File "", line 82, characters 6-17
                      Body Range: File "", line 82, characters 19-20
                      Content: |core: nat -> nat -> nat|
                      references: []
                      (xor#103 -> xor)
                      Range: File "", line 79, characters 6-9
                      Body Range: File "", line 79, characters 11-12
                      Content: |core: nat -> nat -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Bytes#326 -> Bytes)
    Range: File "", line 179, characters 7-12
    Body Range: File "", line 179, character 0 to line 187, character 3
    Content: Members: Variable definitions:
                      (concat#321 -> concat)
                      Range: File "", line 185, characters 6-12
                      Body Range: File "", line 185, characters 14-16
                      Content: |core: bytes -> bytes -> bytes|
                      references: []
                      (concats#312 -> concats)
                      Range: File "", line 180, characters 6-13
                      Body Range: File "", line 180, characters 15-17
                      Content: |core: list (bytes) -> bytes|
                      references: []
                      (length#318 -> length)
                      Range: File "", line 183, characters 6-12
                      Body Range: File "", line 183, characters 14-15
                      Content: |core: bytes -> nat|
                      references: []
                      (pack#314 -> pack)
                      Range: File "", line 181, characters 6-10
                      Body Range: File "", line 181, characters 11-19
                      Content: |core: ∀ a : * . a -> bytes|
                      references: []
                      (sub#325 -> sub)
                      Range: File "", line 186, characters 6-9
                      Body Range: File "", line 186, characters 11-12
                      Content: |core: nat -> nat -> bytes -> bytes|
                      references: []
                      (unpack#316 -> unpack)
                      Range: File "", line 182, characters 6-12
                      Body Range: File "", line 182, characters 13-21
                      Content: |core: ∀ a : * . bytes -> option (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Crypto#343 -> Crypto)
    Range: File "", line 189, characters 7-13
    Body Range: File "", line 189, character 0 to line 197, character 3
    Content: Members: Variable definitions:
                      (blake2b#328 -> blake2b)
                      Range: File "", line 190, characters 6-13
                      Body Range: File "", line 190, characters 15-16
                      Content: |core: bytes -> bytes|
                      references: []
                      (check#342 -> check)
                      Range: File "", line 196, characters 6-11
                      Body Range: File "", line 196, characters 13-14
                      Content: |core: key -> signature -> bytes -> bool|
                      references: []
                      (hash_key#338 -> hash_key)
                      Range: File "", line 195, characters 6-14
                      Body Range: File "", line 195, characters 16-17
                      Content: |core: key -> key_hash|
                      references: []
                      (keccak#336 -> keccak)
                      Range: File "", line 194, characters 6-12
                      Body Range: File "", line 194, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha256#330 -> sha256)
                      Range: File "", line 191, characters 6-12
                      Body Range: File "", line 191, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha3#334 -> sha3)
                      Range: File "", line 193, characters 6-10
                      Body Range: File "", line 193, characters 12-13
                      Content: |core: bytes -> bytes|
                      references: []
                      (sha512#332 -> sha512)
                      Range: File "", line 192, characters 6-12
                      Body Range: File "", line 192, characters 14-15
                      Content: |core: bytes -> bytes|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (List#275 -> List)
    Range: File "", line 138, characters 7-11
    Body Range: File "", line 138, character 0 to line 158, character 3
    Content: Members: Variable definitions:
                      (cons#255 -> cons)
                      Range: File "", line 149, characters 6-10
                      Body Range: File "", line 149, characters 11-19
                      Content: |core: ∀ a : * . a -> list (a) -> list (a)|
                      references: []
                      (filter_map#264 -> filter_map)
                      Range: File "", line 152, characters 6-16
                      Body Range: File "", line 152, characters 17-27
                      Content: |core: ∀ a : * . ∀ b : * . a -> option (b) -> list (a) -> list (b)|
                      references: []
                      (find_opt#259 -> find_opt)
                      Range: File "", line 150, characters 6-14
                      Body Range: File "", line 150, characters 15-23
                      Content: |core: ∀ a : * . a -> bool -> list (a) -> option (a)|
                      references: []
                      (fold#244 -> fold)
                      Range: File "", line 146, characters 6-10
                      Body Range: File "", line 146, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> list (a) -> b -> b|
                      references: File "", line 333, characters 9-13
                      (fold_left#248 -> fold_left)
                      Range: File "", line 147, characters 6-15
                      Body Range: File "", line 147, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> b -> list (a) -> b|
                      references: []
                      (fold_right#252 -> fold_right)
                      Range: File "", line 148, characters 6-16
                      Body Range: File "", line 148, characters 17-27
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> list (a) -> b -> b|
                      references:
                        File "", line 151, characters 4-14 ,
                        File "", line 153, characters 4-14
                      (head_opt#231 -> head_opt)
                      Range: File "", line 141, characters 6-14
                      Body Range: File "", line 141, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (a)|
                      references: []
                      (iter#240 -> iter)
                      Range: File "", line 145, characters 6-10
                      Body Range: File "", line 145, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> list (a) -> unit|
                      references: []
                      (length#226 -> length)
                      Range: File "", line 139, characters 6-12
                      Body Range: File "", line 139, characters 13-21
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (map#237 -> map)
                      Range: File "", line 144, characters 6-9
                      Body Range: File "", line 144, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> list (a) -> list (b)|
                      references:
                        File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 7-10 ,
                        File "", line 155, characters 4-7 ,
                        File "", line 157, characters 4-7
                      (size#228 -> size)
                      Range: File "", line 140, characters 6-10
                      Body Range: File "", line 140, characters 11-19
                      Content: |core: ∀ a : * . list (a) -> nat|
                      references: []
                      (tail_opt#234 -> tail_opt)
                      Range: File "", line 142, characters 6-14
                      Body Range: File "", line 142, characters 15-23
                      Content: |core: ∀ a : * . list (a) -> option (list (a))|
                      references: []
                      (update#269 -> update)
                      Range: File "", line 154, characters 6-12
                      Body Range: File "", line 154, characters 13-21
                      Content: |core: ∀ a : * . a -> option (a) -> list (a) -> list (a)|
                      references: []
                      (update_with#274 -> update_with)
                      Range: File "", line 156, characters 6-17
                      Body Range: File "", line 156, characters 18-26
                      Content: |core: ∀ a : * . a -> bool -> a -> list (a) -> list (a)|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "../../test/contracts/get_scope_tests/constant.mligo", line 4, characters 2-6 ,
      File "", line 333, characters 4-8

    (Map#181 -> Map)
    Range: File "", line 99, characters 7-10
    Body Range: File "", line 99, character 0 to line 115, character 3
    Content: Members: Variable definitions:
                      (add#153 -> add)
                      Range: File "", line 105, characters 6-9
                      Body Range: File "", line 105, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> v -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (empty#142 -> empty)
                      Range: File "", line 100, characters 6-11
                      Body Range: File "", line 100, characters 12-22
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v)|
                      references: []
                      (find#167 -> find)
                      Range: File "", line 109, characters 6-10
                      Body Range: File "", line 109, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> v|
                      references: []
                      (find_opt#170 -> find_opt)
                      Range: File "", line 110, characters 6-14
                      Body Range: File "", line 110, characters 15-25
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> option (v)|
                      references: []
                      (fold#180 -> fold)
                      Range: File "", line 113, characters 6-10
                      Body Range: File "", line 113, characters 11-23
                      Content: |core: ∀ k : * . ∀ v : * . ∀ c : * .
                      ( c *
                        ( k * v ) ) -> c -> map (k ,
                      v) -> c -> c|
                      references: []
                      (get_and_update#164 -> get_and_update)
                      Range: File "", line 108, characters 6-20
                      Body Range: File "", line 108, characters 21-31
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> ( option (v) * map (k , v) )|
                      references: []
                      (iter#173 -> iter)
                      Range: File "", line 111, characters 6-10
                      Body Range: File "", line 111, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . ( k * v ) -> unit -> map (k ,
                      v) -> unit|
                      references: []
                      (literal#146 -> literal)
                      Range: File "", line 102, characters 25-32
                      Body Range: File "", line 102, characters 33-43
                      Content: |core: ∀ k : * . ∀ v : * . list (( k * v )) -> map (k ,
                      v)|
                      references: []
                      (map#176 -> map)
                      Range: File "", line 112, characters 6-9
                      Body Range: File "", line 112, characters 10-22
                      Content: |core: ∀ k : * . ∀ v : * . ∀ w : * .
                      ( k *
                        v ) -> w -> map (k ,
                      v) -> map (k ,
                      w)|
                      references: []
                      (mem#149 -> mem)
                      Range: File "", line 104, characters 6-9
                      Body Range: File "", line 104, characters 10-20
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> bool|
                      references: []
                      (remove#156 -> remove)
                      Range: File "", line 106, characters 6-12
                      Body Range: File "", line 106, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      (size#144 -> size)
                      Range: File "", line 101, characters 6-10
                      Body Range: File "", line 101, characters 11-21
                      Content: |core: ∀ k : * . ∀ v : * . map (k ,
                      v) -> nat|
                      references: []
                      (update#160 -> update)
                      Range: File "", line 107, characters 6-12
                      Body Range: File "", line 107, characters 13-23
                      Content: |core: ∀ k : * . ∀ v : * . k -> option (v) -> map (k ,
                      v) -> map (k ,
                      v)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Option#310 -> Option)
    Range: File "", line 168, characters 7-13
    Body Range: File "", line 168, character 0 to line 177, character 3
    Content: Members: Variable definitions:
                      (is_none#307 -> is_none)
                      Range: File "", line 175, characters 6-13
                      Body Range: File "", line 175, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (is_some#309 -> is_some)
                      Range: File "", line 176, characters 6-13
                      Body Range: File "", line 176, characters 14-22
                      Content: |core: ∀ a : * . option (a) -> bool|
                      references: []
                      (map#297 -> map)
                      Range: File "", line 172, characters 15-18
                      Body Range: File "", line 172, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> option (a) -> option (b)|
                      references: []
                      (unopt#290 -> unopt)
                      Range: File "", line 169, characters 6-11
                      Body Range: File "", line 169, characters 12-20
                      Content: |core: ∀ a : * . option (a) -> a|
                      references: []
                      (unopt_with_error#294 -> unopt_with_error)
                      Range: File "", line 171, characters 6-22
                      Body Range: File "", line 171, characters 23-31
                      Content: |core: ∀ a : * . option (a) -> string -> a|
                      references: []
                      (value#301 -> value)
                      Range: File "", line 173, characters 6-11
                      Body Range: File "", line 173, characters 12-20
                      Content: |core: ∀ a : * . a -> option (a) -> a|
                      references: []
                      (value_exn#305 -> value_exn)
                      Range: File "", line 174, characters 6-15
                      Body Range: File "", line 174, characters 16-28
                      Content: |core: ∀ err : * . ∀ a : * . err -> option (a) -> a|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (Set#224 -> Set)
    Range: File "", line 123, characters 7-10
    Body Range: File "", line 123, character 0 to line 136, character 3
    Content: Members: Variable definitions:
                      (add#205 -> add)
                      Range: File "", line 130, characters 6-9
                      Body Range: File "", line 130, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (cardinal#197 -> cardinal)
                      Range: File "", line 126, characters 6-14
                      Body Range: File "", line 126, characters 15-23
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (empty#193 -> empty)
                      Range: File "", line 124, characters 6-11
                      Body Range: File "", line 124, characters 12-20
                      Content: |core: ∀ a : * . set (a)|
                      references: []
                      (fold#219 -> fold)
                      Range: File "", line 134, characters 6-10
                      Body Range: File "", line 134, characters 11-21
                      Content: |core: ∀ a : * . ∀ b : * . ( b * a ) -> b -> set (a) -> b -> b|
                      references: []
                      (fold_desc#223 -> fold_desc)
                      Range: File "", line 135, characters 6-15
                      Body Range: File "", line 135, characters 16-26
                      Content: |core: ∀ a : * . ∀ b : * . ( a * b ) -> b -> set (a) -> b -> b|
                      references: []
                      (iter#215 -> iter)
                      Range: File "", line 133, characters 6-10
                      Body Range: File "", line 133, characters 11-19
                      Content: |core: ∀ a : * . a -> unit -> set (a) -> unit|
                      references: []
                      (literal#199 -> literal)
                      Range: File "", line 127, characters 25-32
                      Body Range: File "", line 127, characters 33-41
                      Content: |core: ∀ a : * . list (a) -> set (a)|
                      references: []
                      (mem#202 -> mem)
                      Range: File "", line 129, characters 6-9
                      Body Range: File "", line 129, characters 10-18
                      Content: |core: ∀ a : * . a -> set (a) -> bool|
                      references: []
                      (remove#208 -> remove)
                      Range: File "", line 131, characters 6-12
                      Body Range: File "", line 131, characters 13-21
                      Content: |core: ∀ a : * . a -> set (a) -> set (a)|
                      references: []
                      (size#195 -> size)
                      Range: File "", line 125, characters 6-10
                      Body Range: File "", line 125, characters 11-19
                      Content: |core: ∀ a : * . set (a) -> nat|
                      references: []
                      (update#212 -> update)
                      Range: File "", line 132, characters 6-12
                      Body Range: File "", line 132, characters 13-21
                      Content: |unresolved|
                      references: []
                      Type definitions:
                      Module definitions:

    references: []

    (String#287 -> String)
    Range: File "", line 160, characters 7-13
    Body Range: File "", line 160, character 0 to line 166, character 3
    Content: Members: Variable definitions:
                      (concat#282 -> concat)
                      Range: File "", line 164, characters 6-12
                      Body Range: File "", line 164, characters 14-16
                      Content: |core: string -> string -> string|
                      references: []
                      (concats#279 -> concats)
                      Range: File "", line 162, characters 6-13
                      Body Range: File "", line 162, characters 15-17
                      Content: |core: list (string) -> string|
                      references: []
                      (length#277 -> length)
                      Range: File "", line 161, characters 6-12
                      Body Range: File "", line 161, characters 14-15
                      Content: |core: string -> nat|
                      references:
                        File "", line 363, characters 22-28 ,
                        File "", line 366, characters 43-49
                      (sub#286 -> sub)
                      Range: File "", line 165, characters 6-9
                      Body Range: File "", line 165, characters 11-12
                      Content: |core: nat -> nat -> string -> string|
                      references:
                        File "", line 364, characters 24-27 ,
                        File "", line 366, characters 23-26
                      Type definitions:
                      Module definitions:

    references:
      File "", line 363, characters 15-21 ,
      File "", line 364, characters 17-23 ,
      File "", line 366, characters 16-22 ,
      File "", line 366, characters 36-42

    (Test#721 -> Test)
    Range: File "", line 238, characters 7-11
    Body Range: File "", line 238, character 0 to line 476, character 3
    Content: Members: Variable definitions:
                      (add_account#538 -> add_account)
                      Range: File "", line 346, characters 6-17
                      Body Range: File "", line 346, characters 19-20
                      Content: |core: string -> key -> unit|
                      references: []
                      (assert#707 -> assert)
                      Range: File "", line 468, characters 6-12
                      Body Range: File "", line 468, characters 14-15
                      Content: |core: bool -> unit|
                      references: []
                      (assert_none#711 -> assert_none)
                      Range: File "", line 470, characters 6-17
                      Body Range: File "", line 470, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_none_with_error#720 -> assert_none_with_error)
                      Range: File "", line 474, characters 6-28
                      Body Range: File "", line 474, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_some#709 -> assert_some)
                      Range: File "", line 469, characters 6-17
                      Body Range: File "", line 469, characters 18-26
                      Content: |core: ∀ a : * . option (a) -> unit|
                      references: []
                      (assert_some_with_error#717 -> assert_some_with_error)
                      Range: File "", line 473, characters 6-28
                      Body Range: File "", line 473, characters 29-37
                      Content: |core: ∀ a : * . option (a) -> string -> unit|
                      references: []
                      (assert_with_error#714 -> assert_with_error)
                      Range: File "", line 472, characters 6-23
                      Body Range: File "", line 472, characters 25-26
                      Content: |unresolved|
                      references: []
                      (bake_until_n_cycle_end#431 -> bake_until_n_cycle_end)
                      Range: File "", line 265, characters 6-28
                      Body Range: File "", line 265, characters 30-31
                      Content: |core: nat -> unit|
                      references: []
                      (baker_account#541 -> baker_account)
                      Range: File "", line 347, characters 6-19
                      Body Range: File "", line 347, characters 21-22
                      Content: |core: ( string * key ) -> option (tez) -> unit|
                      references: []
                      (bootstrap_contract#526 -> bootstrap_contract)
                      Range: File "", line 342, characters 6-24
                      Body Range: File "", line 342, characters 25-35
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> unit|
                      references: []
                      (cast_address#435 -> cast_address)
                      Range: File "", line 267, characters 6-18
                      Body Range: File "", line 267, characters 19-29
                      Content: |core: ∀ a : * . ∀ b : * . address -> typed_address (a ,
                      b)|
                      references:
                        File "", line 376, characters 35-47 ,
                        File "", line 386, characters 35-47 ,
                        File "", line 393, characters 35-47
                      (chr#475 -> chr)
                      Range: File "", line 291, characters 6-9
                      Body Range: File "", line 291, characters 11-12
                      Content: |core: nat -> option (string)|
                      references: []
                      (compile_contract#470 -> compile_contract)
                      Range: File "", line 286, characters 6-22
                      Body Range: File "", line 286, characters 23-33
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> michelson_contract|
                      references:
                        File "", line 372, characters 12-28 ,
                        File "", line 382, characters 12-28
                      (compile_contract_from_file#612 -> compile_contract_from_file)
                      Range: File "", line 395, characters 6-32
                      Body Range: File "", line 395, characters 34-36
                      Content: |core: string -> string -> list (string) -> michelson_contract|
                      references: File "", line 399, characters 12-38
                      (compile_contract_with_views#588 -> compile_contract_with_views)
                      Range: File "", line 378, characters 8-35
                      Body Range: File "", line 378, characters 36-46
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> views (s) -> michelson_contract|
                      references: File "", line 389, characters 12-39
                      (compile_value#393 -> compile_value)
                      Range: File "", line 243, characters 6-19
                      Body Range: File "", line 243, characters 20-28
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references: []
                      (constant_to_michelson_program#443 -> constant_to_michelson_program)
                      Range: File "", line 271, characters 6-35
                      Body Range: File "", line 271, characters 37-38
                      Content: |core: string -> michelson_program|
                      references: []
                      (create_chest#547 -> create_chest)
                      Range: File "", line 349, characters 6-18
                      Body Range: File "", line 349, characters 20-21
                      Content: |core: bytes -> nat -> ( chest * chest_key )|
                      references: []
                      (create_chest_key#550 -> create_chest_key)
                      Range: File "", line 350, characters 6-22
                      Body Range: File "", line 350, characters 24-25
                      Content: |core: chest -> nat -> chest_key|
                      references: []
                      (decompile#429 -> decompile)
                      Range: File "", line 264, characters 6-15
                      Body Range: File "", line 264, characters 16-24
                      Content: |core: ∀ a : * . michelson_program -> a|
                      references: File "", line 282, characters 5-14
                      (drop_context#451 -> drop_context)
                      Range: File "", line 275, characters 6-18
                      Body Range: File "", line 275, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (eprint#409 -> eprint)
                      Range: File "", line 251, characters 6-12
                      Body Range: File "", line 251, characters 14-15
                      Content: |core: string -> unit|
                      references: []
                      (eval#391 -> eval)
                      Range: File "", line 241, characters 6-10
                      Body Range: File "", line 241, characters 11-19
                      Content: |core: ∀ a : * . a -> michelson_program|
                      references:
                        File "", line 243, characters 59-63 ,
                        File "", line 354, characters 32-36 ,
                        File "", line 359, characters 34-38 ,
                        File "", line 373, characters 12-16 ,
                        File "", line 383, characters 12-16 ,
                        File "", line 390, characters 12-16
                      (failwith#397 -> failwith)
                      Range: File "", line 245, characters 6-14
                      Body Range: File "", line 245, characters 15-25
                      Content: |core: ∀ a : * . ∀ b : * . a -> b|
                      references:
                        File "", line 468, characters 51-59 ,
                        File "", line 469, characters 74-82 ,
                        File "", line 470, characters 89-97 ,
                        File "", line 472, characters 68-76 ,
                        File "", line 473, characters 98-106 ,
                        File "", line 474, characters 113-121
                      (get_balance#405 -> get_balance)
                      Range: File "", line 249, characters 6-17
                      Body Range: File "", line 249, characters 19-20
                      Content: |core: address -> tez|
                      references: []
                      (get_bootstrap_account#418 -> get_bootstrap_account)
                      Range: File "", line 257, characters 6-27
                      Body Range: File "", line 257, characters 29-30
                      Content: |core: nat -> ( address * key * string )|
                      references: []
                      (get_last_events_from#503 -> get_last_events_from)
                      Range: File "", line 326, characters 6-26
                      Body Range: File "", line 326, characters 27-39
                      Content: |core: ∀ a : * . ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> string -> list (a)|
                      references: []
                      (get_storage#460 -> get_storage)
                      Range: File "", line 278, characters 6-17
                      Body Range: File "", line 278, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> s|
                      references: []
                      (get_storage_of_address#403 -> get_storage_of_address)
                      Range: File "", line 248, characters 6-28
                      Body Range: File "", line 248, characters 30-31
                      Content: |core: address -> michelson_program|
                      references: File "", line 281, characters 32-54
                      (get_time#433 -> get_time)
                      Range: File "", line 266, characters 6-14
                      Body Range: File "", line 266, characters 16-18
                      Content: |core: unit -> timestamp|
                      references: []
                      (get_total_voting_power#395 -> get_total_voting_power)
                      Range: File "", line 244, characters 6-28
                      Body Range: File "", line 244, characters 30-32
                      Content: |core: unit -> nat|
                      references: []
                      (get_voting_power#411 -> get_voting_power)
                      Range: File "", line 252, characters 6-22
                      Body Range: File "", line 252, characters 24-26
                      Content: |core: key_hash -> nat|
                      references: []
                      (last_originations#422 -> last_originations)
                      Range: File "", line 259, characters 6-23
                      Body Range: File "", line 259, characters 25-26
                      Content: |core: unit -> map (address ,
                      list (address))|
                      references: []
                      (log#515 -> log)
                      Range: File "", line 336, characters 6-9
                      Body Range: File "", line 336, characters 10-18
                      Content: |core: ∀ a : * . a -> unit|
                      references: File "", line 365, characters 25-28
                      (michelson_equal#567 -> michelson_equal)
                      Range: File "", line 361, characters 6-21
                      Body Range: File "", line 361, characters 23-25
                      Content: |core: michelson_program -> michelson_program -> bool|
                      references: []
                      (mutate_value#529 -> mutate_value)
                      Range: File "", line 343, characters 6-18
                      Body Range: File "", line 343, characters 19-27
                      Content: |core: ∀ a : * . nat -> a -> option (( a *
                                                                        mutation ))|
                      references:
                        File "", line 407, characters 23-35 ,
                        File "", line 419, characters 23-35
                      (mutation_test#636 -> mutation_test)
                      Range: File "", line 403, characters 6-19
                      Body Range: File "", line 403, characters 20-30
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> option (
                      ( b *
                        mutation ))|
                      references: []
                      (mutation_test_all#652 -> mutation_test_all)
                      Range: File "", line 415, characters 6-23
                      Body Range: File "", line 415, characters 24-34
                      Content: |core: ∀ a : * . ∀ b : * . a -> a -> b -> list (
                      ( b *
                        mutation ))|
                      references: []
                      (new_account#427 -> new_account)
                      Range: File "", line 263, characters 6-17
                      Body Range: File "", line 263, characters 19-20
                      Content: |core: unit -> ( string * key )|
                      references: []
                      (nl#476 -> nl)
                      Range: File "", line 301, characters 6-8
                      Body Range: File "", line 301, characters 11-53
                      Content: |unresolved|
                      references: File "", line 303, characters 15-17
                      (nth_bootstrap_account#416 -> nth_bootstrap_account)
                      Range: File "", line 254, characters 6-27
                      Body Range: File "", line 254, characters 29-30
                      Content: |core: int -> address|
                      references: []
                      (nth_bootstrap_contract#413 -> nth_bootstrap_contract)
                      Range: File "", line 253, characters 6-28
                      Body Range: File "", line 253, characters 30-31
                      Content: |core: nat -> address|
                      references: []
                      (nth_bootstrap_typed_address#420 -> nth_bootstrap_typed_address)
                      Range: File "", line 258, characters 6-33
                      Body Range: File "", line 258, characters 34-44
                      Content: |core: ∀ a : * . ∀ b : * . nat -> typed_address (a ,
                      b)|
                      references: []
                      (originate#584 -> originate)
                      Range: File "", line 371, characters 6-15
                      Body Range: File "", line 371, characters 16-26
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_contract#575 -> originate_contract)
                      Range: File "", line 370, characters 6-24
                      Body Range: File "", line 370, characters 26-27
                      Content: |core: michelson_contract -> michelson_program -> tez -> address|
                      references:
                        File "", line 374, characters 12-30 ,
                        File "", line 384, characters 12-30 ,
                        File "", line 391, characters 12-30 ,
                        File "", line 400, characters 12-30 ,
                        File "", line 431, characters 14-32 ,
                        File "", line 451, characters 14-32
                      (originate_from_file#621 -> originate_from_file)
                      Range: File "", line 398, characters 6-25
                      Body Range: File "", line 398, characters 27-29
                      Content: |core: string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_from_file_and_mutate#678 -> originate_from_file_and_mutate)
                      Range: File "", line 427, characters 6-36
                      Body Range: File "", line 427, characters 37-45
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> option (( b * mutation ))|
                      references: []
                      (originate_from_file_and_mutate_all#705 -> originate_from_file_and_mutate_all)
                      Range: File "", line 447, characters 6-40
                      Body Range: File "", line 447, characters 41-49
                      Content: |core: ∀ b : * . string -> string -> list (string) -> michelson_program -> tez ->
                      ( address *
                        michelson_contract *
                        int ) -> b -> list (( b * mutation ))|
                      references: []
                      (originate_module#607 -> originate_module)
                      Range: File "", line 388, characters 6-22
                      Body Range: File "", line 388, characters 23-33
                      Content: |core: ∀ p : * . ∀ s : * . ( ( p * s ) ->
                                                                ( list (operation) *
                                                                  s ) *
                                                                views (s) ) -> s -> tez ->
                      ( typed_address (p ,
                        s) *
                        michelson_contract *
                        int )|
                      references: []
                      (originate_uncurried#597 -> originate_uncurried)
                      Range: File "", line 381, characters 6-25
                      Body Range: File "", line 381, characters 26-36
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> s -> tez -> ( typed_address (p ,
                                             s) *
                                             michelson_contract *
                                             int )|
                      references: []
                      (parse_michelson#445 -> parse_michelson)
                      Range: File "", line 272, characters 6-21
                      Body Range: File "", line 272, characters 23-24
                      Content: |core: string -> michelson_program|
                      references: []
                      (print#407 -> print)
                      Range: File "", line 250, characters 6-11
                      Body Range: File "", line 250, characters 13-14
                      Content: |core: string -> unit|
                      references:
                        File "", line 303, characters 4-9 ,
                        File "", line 339, characters 4-9
                      (println#478 -> println)
                      Range: File "", line 302, characters 6-13
                      Body Range: File "", line 302, characters 15-16
                      Content: |core: string -> unit|
                      references: []
                      (random#425 -> random)
                      Range: File "", line 260, characters 6-12
                      Body Range: File "", line 260, characters 13-21
                      Content: |core: ∀ a : * . unit -> a|
                      references: []
                      (read_contract_from_file#472 -> read_contract_from_file)
                      Range: File "", line 290, characters 6-29
                      Body Range: File "", line 290, characters 31-33
                      Content: |core: string -> michelson_contract|
                      references: []
                      (register_constant#439 -> register_constant)
                      Range: File "", line 269, characters 6-23
                      Body Range: File "", line 269, characters 25-26
                      Content: |core: michelson_program -> string|
                      references: []
                      (register_delegate#437 -> register_delegate)
                      Range: File "", line 268, characters 6-23
                      Body Range: File "", line 268, characters 25-27
                      Content: |core: key_hash -> unit|
                      references: []
                      (reset_state#518 -> reset_state)
                      Range: File "", line 340, characters 6-17
                      Body Range: File "", line 340, characters 19-20
                      Content: |core: nat -> list (tez) -> unit|
                      references: []
                      (reset_state_at#522 -> reset_state_at)
                      Range: File "", line 341, characters 6-20
                      Body Range: File "", line 341, characters 22-23
                      Content: |core: timestamp -> nat -> list (tez) -> unit|
                      references: []
                      (restore_context#447 -> restore_context)
                      Range: File "", line 273, characters 6-21
                      Body Range: File "", line 273, characters 23-24
                      Content: |core: unit -> unit|
                      references: []
                      (run#388 -> run)
                      Range: File "", line 240, characters 6-9
                      Body Range: File "", line 240, characters 10-20
                      Content: |core: ∀ a : * . ∀ b : * . a -> b -> a -> michelson_program|
                      references: File "", line 241, characters 50-53
                      (save_context#449 -> save_context)
                      Range: File "", line 274, characters 6-18
                      Body Range: File "", line 274, characters 20-21
                      Content: |core: unit -> unit|
                      references: []
                      (save_mutation#532 -> save_mutation)
                      Range: File "", line 344, characters 6-19
                      Body Range: File "", line 344, characters 21-22
                      Content: |core: string -> mutation -> option (string)|
                      references: []
                      (set_baker#464 -> set_baker)
                      Range: File "", line 284, characters 6-15
                      Body Range: File "", line 284, characters 17-18
                      Content: |core: address -> unit|
                      references: []
                      (set_baker_policy#462 -> set_baker_policy)
                      Range: File "", line 283, characters 6-22
                      Body Range: File "", line 283, characters 24-26
                      Content: |core: test_baker_policy -> unit|
                      references: File "", line 284, characters 39-55
                      (set_big_map#544 -> set_big_map)
                      Range: File "", line 348, characters 6-17
                      Body Range: File "", line 348, characters 18-28
                      Content: |core: ∀ a : * . ∀ b : * . int -> big_map (a ,
                      b) -> unit|
                      references: []
                      (set_print_values#479 -> set_print_values)
                      Range: File "", line 305, characters 6-22
                      Body Range: File "", line 305, characters 24-25
                      Content: |core: unit -> unit|
                      references: []
                      (set_source#401 -> set_source)
                      Range: File "", line 247, characters 6-16
                      Body Range: File "", line 247, characters 18-19
                      Content: |core: address -> unit|
                      references: []
                      (sign#535 -> sign)
                      Range: File "", line 345, characters 6-10
                      Body Range: File "", line 345, characters 12-14
                      Content: |core: string -> bytes -> signature|
                      references: []
                      (size#466 -> size)
                      Range: File "", line 285, characters 6-10
                      Body Range: File "", line 285, characters 12-13
                      Content: |core: michelson_contract -> int|
                      references:
                        File "", line 375, characters 12-16 ,
                        File "", line 385, characters 12-16 ,
                        File "", line 392, characters 12-16 ,
                        File "", line 401, characters 12-16 ,
                        File "", line 432, characters 14-18 ,
                        File "", line 452, characters 14-18
                      (to_contract#399 -> to_contract)
                      Range: File "", line 246, characters 6-17
                      Body Range: File "", line 246, characters 18-28
                      Content: |core: ∀ p : * . ∀ s : * . typed_address (p ,
                      s) -> contract (p)|
                      references:
                        File "", line 279, characters 25-36 ,
                        File "", line 327, characters 30-41
                      (to_entrypoint#571 -> to_entrypoint)
                      Range: File "", line 362, characters 6-19
                      Body Range: File "", line 362, characters 20-32
                      Content: |core: ∀ a : * . ∀ b : * . ∀ c : * . string -> typed_address (a ,
                      b) -> contract (c)|
                      references: []
                      (to_json#455 -> to_json)
                      Range: File "", line 277, characters 6-13
                      Body Range: File "", line 277, characters 14-22
                      Content: |core: ∀ a : * . a -> string|
                      references: []
                      (to_string#453 -> to_string)
                      Range: File "", line 276, characters 6-15
                      Body Range: File "", line 276, characters 16-24
                      Content: |core: ∀ a : * . a -> string|
                      references:
                        File "", line 294, characters 68-77 ,
                        File "", line 296, characters 67-76 ,
                        File "", line 298, characters 61-70 ,
                        File "", line 338, characters 12-21
                      (to_typed_address#441 -> to_typed_address)
                      Range: File "", line 270, characters 6-22
                      Body Range: File "", line 270, characters 23-33
                      Content: |core: ∀ a : * . ∀ b : * . contract (a) -> typed_address (a ,
                      b)|
                      references: []
                      (transfer#507 -> transfer)
                      Range: File "", line 334, characters 6-14
                      Body Range: File "", line 334, characters 16-17
                      Content: |core: address -> michelson_program -> tez -> test_exec_result|
                      references: []
                      (transfer_exn#511 -> transfer_exn)
                      Range: File "", line 335, characters 6-18
                      Body Range: File "", line 335, characters 20-21
                      Content: |core: address -> michelson_program -> tez -> nat|
                      references: []
                      (transfer_to_contract#557 -> transfer_to_contract)
                      Range: File "", line 351, characters 6-26
                      Body Range: File "", line 351, characters 27-35
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> test_exec_result|
                      references: []
                      (transfer_to_contract_exn#564 -> transfer_to_contract_exn)
                      Range: File "", line 356, characters 6-30
                      Body Range: File "", line 356, characters 31-39
                      Content: |core: ∀ p : * . contract (p) -> p -> tez -> nat|
                      references: []
                      (unset_print_values#480 -> unset_print_values)
                      Range: File "", line 306, characters 6-24
                      Body Range: File "", line 306, characters 26-27
                      Content: |core: unit -> unit|
                      references: []
                      Type definitions:
                      Module definitions:
                      (PBT#494 -> PBT)
                      Range: File "", line 308, characters 9-12
                      Body Range: File "", line 308, character 2 to line 324, character 5
                      Content: Members: Variable definitions:
                                        (gen#481 -> gen)
                                        Range: File "", line 309, characters 8-11
                                        Body Range: File "", line 309, characters 12-20
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (gen_small#482 -> gen_small)
                                        Range: File "", line 310, characters 8-17
                                        Body Range: File "", line 310, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a)|
                                        references: []
                                        (make_test#485 -> make_test)
                                        Range: File "", line 311, characters 8-17
                                        Body Range: File "", line 311, characters 18-26
                                        Content: |core: ∀ a : * . pbt_gen (a) -> a -> bool -> pbt_test (a)|
                                        references: []
                                        (run#493 -> run)
                                        Range: File "", line 312, characters 8-11
                                        Body Range: File "", line 312, characters 12-20
                                        Content: |core: ∀ a : * . pbt_test (a) -> nat -> pbt_result (a)|
                                        references: []
                                        Type definitions:
                                        Module definitions:

                      references: []


    references: []

    (Tezos#97 -> Tezos)
    Range: File "", line 7, characters 7-12
    Body Range: File "", line 7, character 0 to line 75, character 3
    Content: Members: Variable definitions:
                      (address#27 -> address)
                      Range: File "", line 20, characters 6-13
                      Body Range: File "", line 20, characters 14-22
                      Content: |core: ∀ a : * . contract (a) -> address|
                      references: File "", line 327, characters 21-28
                      (call_view#66 -> call_view)
                      Range: File "", line 55, characters 25-34
                      Body Range: File "", line 55, characters 35-45
                      Content: |core: ∀ a : * . ∀ b : * . string -> a -> address -> option (b)|
                      references: []
                      (constant#43 -> constant)
                      Range: File "", line 31, characters 25-33
                      Body Range: File "", line 31, characters 34-42
                      Content: |core: ∀ a : * . string -> a|
                      references: []
                      (create_contract#77 -> create_contract)
                      Range: File "", line 59, characters 25-40
                      Body Range: File "", line 59, characters 41-51
                      Content: |core: ∀ p : * . ∀ s : * . p -> s -> ( list (operation) *
                                                                        s ) -> option (key_hash) -> tez -> s ->
                      ( operation *
                        address )|
                      references: []
                      (create_contract_uncurried#82 -> create_contract_uncurried)
                      Range: File "", line 62, characters 25-50
                      Body Range: File "", line 62, characters 51-61
                      Content: |core: ∀ p : * . ∀ s : * . ( p * s ) ->
                      ( list (operation) *
                        s ) -> option (key_hash) -> tez -> s -> ( operation *
                                                                  address )|
                      references: []
                      (create_ticket#58 -> create_ticket)
                      Range: File "", line 47, characters 6-19
                      Body Range: File "", line 47, characters 20-28
                      Content: |core: ∀ a : * . a -> nat -> option (ticket (a))|
                      references: []
                      (emit#93 -> emit)
                      Range: File "", line 70, characters 25-29
                      Body Range: File "", line 70, characters 30-38
                      Content: |core: ∀ a : * . string -> a -> operation|
                      references: []
                      (get_amount#7 -> get_amount)
                      Range: File "", line 10, characters 6-16
                      Body Range: File "", line 10, characters 18-20
                      Content: |core: unit -> tez|
                      references: []
                      (get_balance#5 -> get_balance)
                      Range: File "", line 9, characters 6-17
                      Body Range: File "", line 9, characters 19-21
                      Content: |core: unit -> tez|
                      references: []
                      (get_chain_id#19 -> get_chain_id)
                      Range: File "", line 16, characters 6-18
                      Body Range: File "", line 16, characters 20-22
                      Content: |core: unit -> chain_id|
                      references: []
                      (get_contract#50 -> get_contract)
                      Range: File "", line 36, characters 25-37
                      Body Range: File "", line 36, characters 38-46
                      Content: |core: ∀ a : * . address -> contract (a)|
                      references: []
                      (get_contract_opt#46 -> get_contract_opt)
                      Range: File "", line 34, characters 25-41
                      Body Range: File "", line 34, characters 42-50
                      Content: |core: ∀ p : * . address -> option (contract (p))|
                      references:
                        File "", line 37, characters 12-28 ,
                        File "", line 41, characters 12-28
                      (get_contract_with_error#55 -> get_contract_with_error)
                      Range: File "", line 40, characters 6-29
                      Body Range: File "", line 40, characters 30-38
                      Content: |core: ∀ a : * . address -> string -> contract (a)|
                      references: []
                      (get_entrypoint#90 -> get_entrypoint)
                      Range: File "", line 67, characters 25-39
                      Body Range: File "", line 67, characters 40-48
                      Content: |core: ∀ p : * . string -> address -> contract (p)|
                      references: []
                      (get_entrypoint_opt#85 -> get_entrypoint_opt)
                      Range: File "", line 64, characters 25-43
                      Body Range: File "", line 64, characters 44-52
                      Content: |core: ∀ p : * . string -> address -> option (contract (p))|
                      references: File "", line 68, characters 12-30
                      (get_level#15 -> get_level)
                      Range: File "", line 14, characters 6-15
                      Body Range: File "", line 14, characters 17-19
                      Content: |core: unit -> nat|
                      references: []
                      (get_min_block_time#23 -> get_min_block_time)
                      Range: File "", line 18, characters 6-24
                      Body Range: File "", line 18, characters 26-28
                      Content: |core: unit -> nat|
                      references: []
                      (get_now#9 -> get_now)
                      Range: File "", line 11, characters 6-13
                      Body Range: File "", line 11, characters 15-17
                      Content: |core: unit -> timestamp|
                      references: File "", line 266, characters 47-54
                      (get_self_address#17 -> get_self_address)
                      Range: File "", line 15, characters 6-22
                      Body Range: File "", line 15, characters 24-26
                      Content: |core: unit -> address|
                      references: []
                      (get_sender#11 -> get_sender)
                      Range: File "", line 12, characters 6-16
                      Body Range: File "", line 12, characters 18-20
                      Content: |core: unit -> address|
                      references: []
                      (get_source#13 -> get_source)
                      Range: File "", line 13, characters 6-16
                      Body Range: File "", line 13, characters 18-20
                      Content: |core: unit -> address|
                      references: []
                      (get_total_voting_power#21 -> get_total_voting_power)
                      Range: File "", line 17, characters 6-28
                      Body Range: File "", line 17, characters 30-32
                      Content: |core: unit -> nat|
                      references: []
                      (implicit_account#29 -> implicit_account)
                      Range: File "", line 21, characters 6-22
                      Body Range: File "", line 21, characters 24-26
                      Content: |core: key_hash -> contract (unit)|
                      references: []
                      (join_tickets#31 -> join_tickets)
                      Range: File "", line 22, characters 6-18
                      Body Range: File "", line 22, characters 19-27
                      Content: |core: ∀ a : * . ( ticket (a) * ticket (a) ) -> option (ticket (a))|
                      references: []
                      (never#35 -> never)
                      Range: File "", line 25, characters 6-11
                      Body Range: File "", line 25, characters 12-20
                      Content: |core: ∀ a : * . never -> a|
                      references: []
                      (pairing_check#37 -> pairing_check)
                      Range: File "", line 26, characters 6-19
                      Body Range: File "", line 26, characters 21-22
                      Content: |core: list (( bls12_381_g1 * bls12_381_g2 )) -> bool|
                      references: []
                      (read_ticket#33 -> read_ticket)
                      Range: File "", line 23, characters 6-17
                      Body Range: File "", line 23, characters 18-26
                      Content: |core: ∀ a : * . ticket (a) -> ( ( address *
                                                                    ( a * nat ) ) *
                                                                  ticket (a) )|
                      references: []
                      (sapling_empty_state#44 -> sapling_empty_state)
                      Range: File "", line 32, characters 25-44
                      Body Range: File "", line 32, characters 45-57
                      Content: |core: ∀ sap_a : + . sapling_state (sap_a)|
                      references: []
                      (sapling_verify_update#96 -> sapling_verify_update)
                      Range: File "", line 73, characters 25-46
                      Body Range: File "", line 73, characters 47-59
                      Content: |core: ∀ sap_a : + . sapling_transaction (sap_a) -> sapling_state (sap_a) -> option (
                      ( bytes *
                        ( int * sapling_state (sap_a) ) ))|
                      references: []
                      (self#41 -> self)
                      Range: File "", line 28, characters 25-29
                      Body Range: File "", line 28, characters 30-38
                      Content: |core: ∀ a : * . string -> contract (a)|
                      references: []
                      (set_delegate#39 -> set_delegate)
                      Range: File "", line 27, characters 6-18
                      Body Range: File "", line 27, characters 20-21
                      Content: |core: option (key_hash) -> operation|
                      references: []
                      (split_ticket#69 -> split_ticket)
                      Range: File "", line 57, characters 6-18
                      Body Range: File "", line 57, characters 19-27
                      Content: |core: ∀ a : * . ticket (a) -> ( nat * nat ) -> option (
                      ( ticket (a) *
                        ticket (a) ))|
                      references: []
                      (transaction#62 -> transaction)
                      Range: File "", line 49, characters 6-17
                      Body Range: File "", line 49, characters 18-26
                      Content: |core: ∀ a : * . a -> tez -> contract (a) -> operation|
                      references: []
                      (voting_power#25 -> voting_power)
                      Range: File "", line 19, characters 6-18
                      Body Range: File "", line 19, characters 20-22
                      Content: |core: key_hash -> nat|
                      references: []
                      Type definitions:
                      Module definitions:

    references:
      File "", line 266, characters 41-46 ,
      File "", line 327, characters 15-20

    (Transpiled#192 -> Transpiled)
    Range: File "", line 117, characters 7-17
    Body Range: File "", line 117, character 0 to line 121, character 3
    Content: Members: Variable definitions:
                      (map_add#188 -> map_add)
                      Range: File "", line 119, characters 6-13
                      Body Range: File "", line 119, characters 14-26
                      Content: |core: ∀ k : * . ∀ v : * . ∀ b : * . k -> v -> b -> external_map_add (k ,
                      v ,
                      b)|
                      references: []
                      (map_find_opt#184 -> map_find_opt)
                      Range: File "", line 118, characters 6-18
                      Body Range: File "", line 118, characters 19-29
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_find_opt (k ,
                      b)|
                      references: []
                      (map_remove#191 -> map_remove)
                      Range: File "", line 120, characters 6-16
                      Body Range: File "", line 120, characters 17-27
                      Content: |core: ∀ k : * . ∀ b : * . k -> b -> external_map_remove (k ,
                      b)|
                      references: []
                      Type definitions:
                      Module definitions:

    references: [] |}]

let () = Ligo_unix.putenv ~key:"LIGO_GET_SCOPE_USE_NEW_IMP" ~data:""
