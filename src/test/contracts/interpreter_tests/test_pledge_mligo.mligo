#import "../pledge.mligo" "Pledge"

let originate storage balance =
  let storage = Test.eval storage in
  let addr, ctr, size = Test.originate_from_file "../pledge.mligo" "main" [] storage balance in
  ((Test.cast_address addr : (Pledge.parameter, Pledge.storage) typed_address), ctr, size)

let test_pledge =
  let () = Test.reset_state_at (0 : timestamp) 10n ([] : tez list) in
  let oracle_addr = Test.nth_bootstrap_account 0 in
  let init_storage = oracle_addr in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = Donate in
  let () = Test.set_source oracle_addr in
  let _ = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  let post_storage = oracle_addr in
  assert (new_storage = post_storage)

let test_distribute =
  let () = Test.reset_state_at (0 : timestamp) 10n ([] : tez list) in
  let oracle_addr = Test.nth_bootstrap_account 0 in
  let init_storage = oracle_addr in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = Distribute (fun (_ : unit) -> []) in
  let () = Test.set_source oracle_addr in
  let _ = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  let post_storage = oracle_addr in
  assert (new_storage = post_storage)

let test_distribute_unauthorized =
  let () = Test.reset_state_at (0 : timestamp) 10n ([] : tez list) in
  let oracle_addr = Test.nth_bootstrap_account 0 in
  let stranger_addr = Test.nth_bootstrap_account 1 in
  let init_storage = oracle_addr in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = Distribute (fun (_ : unit) -> []) in
  let () = Test.set_source stranger_addr in
  match Test.transfer_to_contract contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> assert (Test.michelson_equal a (Test.eval "You're not the oracle for this distribution."))
  | Fail _ -> failwith "Transaction should fail with rejection"
