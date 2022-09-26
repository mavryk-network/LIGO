#import "../FA1.mligo" "FA1"

let originate storage balance =
  let storage = Test.eval storage in
  let addr, ctr, size = Test.originate_from_file "../FA1.mligo" "main" [] storage balance in
  ((Test.cast_address addr : (FA1.parameter, FA1.storage) typed_address), ctr, size)

let test_transfer =
  let () = Test.reset_state 10n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 0 in
  let from_ = Test.nth_bootstrap_account 1 in
  let to_ = Test.nth_bootstrap_account 2 in
  let storage = { ledger = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
                  totalSupply = 300n } in
  let (typed_addr, _, _) = originate storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = Transfer ((from_, (to_, 10n)) : FA1.transfer) in
  let () = Test.set_source sender_ in
  let _ = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  assert ((Big_map.find_opt to_ new_storage.ledger = Some 110n) &&
          (Big_map.find_opt from_ new_storage.ledger = Some 90n) &&
          (Big_map.find_opt sender_ new_storage.ledger = Some 100n) &&
          (new_storage.totalSupply = 300n))

let test_transfer_not_e_balance =
  let () = Test.reset_state 10n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 0 in
  let from_ = Test.nth_bootstrap_account 1 in
  let to_ = Test.nth_bootstrap_account 2 in
  let storage = { ledger = Big_map.literal [(sender_, 100n); (from_, 0n); (to_, 100n)];
                  totalSupply = 300n } in
  let (typed_addr, _, _) = originate storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = Transfer (from_, (to_, 10n)) in
  let () = Test.set_source sender_ in
  match Test.transfer_to_contract contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> assert (Test.michelson_equal a (Test.eval "NotEnoughBalance"))
  | Fail _ -> failwith "Transaction should fail with rejection"

let test_get_balance =
  let dummy_contract (v, s : nat * nat) : operation list * nat = ([] : operation list), v + s in
  let () = Test.reset_state 10n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 0 in
  let from_ = Test.nth_bootstrap_account 1 in
  let to_ = Test.nth_bootstrap_account 2 in
  let (dummy_typed_addr, _, _) = Test.originate dummy_contract 0n 0tez in
  let dummy_typed_contr = Test.to_contract dummy_typed_addr in
  let storage = { ledger = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
                  totalSupply = 300n } in
  let (typed_addr, _, _) = originate storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = GetBalance (from_, dummy_typed_contr) in
  let () = Test.set_source sender_ in
  let _ = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  let _ = assert ((Big_map.find_opt to_ new_storage.ledger = Some 100n) &&
                  (Big_map.find_opt from_ new_storage.ledger = Some 100n) &&
                  (Big_map.find_opt sender_ new_storage.ledger = Some 100n) &&
                  (new_storage.totalSupply = 300n)) in
  let dummy_new_storage = Test.get_storage dummy_typed_addr in
  assert (dummy_new_storage = 100n)

let test_get_totalSupply =
  let dummy_contract (v, s : nat * nat) : operation list * nat = ([] : operation list), v + s in
  let () = Test.reset_state 10n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 0 in
  let from_ = Test.nth_bootstrap_account 1 in
  let to_ = Test.nth_bootstrap_account 2 in
  let (dummy_typed_addr, _, _) = Test.originate dummy_contract 0n 0tez in
  let dummy_typed_contr = Test.to_contract dummy_typed_addr in
  let storage = { ledger = Big_map.literal [(sender_, 100n); (from_, 100n); (to_, 100n)];
                  totalSupply = 300n } in
  let (typed_addr, _, _) = originate storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = GetTotalSupply ((), dummy_typed_contr) in
  let () = Test.set_source sender_ in
  let _ = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  let _ = assert ((Big_map.find_opt to_ new_storage.ledger = Some 100n) &&
                  (Big_map.find_opt from_ new_storage.ledger = Some 100n) &&
                  (Big_map.find_opt sender_ new_storage.ledger = Some 100n) &&
                  (new_storage.totalSupply = 300n)) in
  let dummy_new_storage = Test.get_storage dummy_typed_addr in
  assert (dummy_new_storage = 300n)
