#import "../vote.mligo" "Vote"

let originate storage balance =
  let storage = Test.eval storage in
  let addr, ctr, size = Test.originate_from_file "../vote.mligo" "main" [] storage balance in
  ((Test.cast_address addr : (Vote.parameter, Vote.storage) typed_address), ctr, size)

let test =
  let () = Test.reset_state 10n ([] : tez list) in
  let sender_ = Test.nth_bootstrap_account 0 in
  let storage : Vote.storage = { title = "basic" ; yea = 0n ; nay = 0n ; voters = Set.empty ; start_time = (0 : timestamp) ; finish_time = (1000000000 : timestamp)  } in
  let (typed_addr, _, _) = originate storage 0tez in
  let contr = Test.to_contract typed_addr in
  let parameter = Vote Yea in
  let () = Test.set_source sender_ in
  let _ = Test.transfer_to_contract_exn contr parameter 0tez in
  let new_storage = Test.get_storage typed_addr in
  assert (new_storage.yea = 1n)
