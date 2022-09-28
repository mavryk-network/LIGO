#import "../time-lock.ligo" "Timelock"

let originate storage balance =
  let storage = Test.eval storage in
  let addr, ctr, size = Test.originate_from_file "../time-lock.ligo" "main" [] storage balance in
  ((Test.cast_address addr : (Timelock.entry_point_t, Timelock.storage_t) typed_address), ctr, size)

let test_early_call =
  let () = Test.reset_state_at (0 : timestamp) 10n ([] : tez list) in
  let init_storage = (3600 : timestamp) in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr : Timelock.call_pt contract = Test.to_entrypoint "call" typed_addr in
  let parameter = fun (_ : unit) -> [] in
  match Test.transfer_to_contract contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> assert (Test.michelson_equal a (Test.eval "Contract is still time locked"))
  | Fail x -> let () = Test.log x in failwith "Transaction should fail with rejection"

let test_call_on_time =
  let () = Test.reset_state_at (3600 : timestamp) 10n ([] : tez list) in
  let init_storage = (0 : timestamp) in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr : Timelock.default_pt contract = Test.to_entrypoint "default" typed_addr in
  let parameter = () in
  let _ = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  let post_storage = (0 : timestamp) in
  assert (new_storage = post_storage)
