#import "../timelock_repeat.mligo" "Timelock"

let originate storage balance =
  let storage = Test.eval storage in
  let addr, ctr, size = Test.originate_from_file "../timelock_repeat.mligo" "main" [] storage balance in
  ((Test.cast_address addr : (Timelock.parameter, Timelock.storage) typed_address), ctr, size)

let test_early_call =
  let () = Test.reset_state_at (0 : timestamp) 10n ([] : tez list) in
  let init_storage : Timelock.storage = { next_use = (3600 : timestamp) ; interval = 86_400 ; execute = fun (_ : unit) -> [] } in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = () in
  match Test.transfer_to_contract contr parameter 0tez with
  | Success _ -> failwith "Transaction should fail"
  | Fail (Rejected (a, _)) -> assert (Test.michelson_equal a (Test.eval "You have to wait before you can execute this contract again."))
  | Fail x -> let () = Test.log x in failwith "Transaction should fail with rejection"

let test_interval_advance =
  let () = Test.reset_state_at (36_000 : timestamp) 10n ([] : tez list) in
  let init_storage : Timelock.storage = { next_use = (0 : timestamp) ; interval = 86_400 ; execute = fun (_ : unit) -> [] } in
  let (typed_addr, _, _) = originate init_storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = () in
  let new_next_use = Tezos.get_now () + 30 + 86_400 in
  let _ = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  assert (new_storage.next_use = new_next_use &&
          new_storage.interval = 86_400 &&
          (match new_storage.execute () with [] -> true | _ -> false))