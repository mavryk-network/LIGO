module IncDec = struct
  type storage = int
  type ret = operation list * storage

  (* Three entrypoints *)

  [@entry]
  let increment (delta : int) (store : storage) : ret = [], store + delta

  [@entry]
  let decrement (delta : int) (store : storage) : ret = [], store - delta

  [@entry]
  let reset (() : unit) (_ : storage) : ret = [], 0

  [@entry]
  let brk (() : unit) (_ : storage) : ret = failwith "oops"
end

(* Tests for main access point *)

let initial_storage = 42

let test_increment =
  let () = Test.println "Increment" in
  let () = Test.reset_state 10n ([] : tez list) in
  let () = Test.log (Test.get_time ()) in
  let (taddr, _, _) =
    Test.originate_module (contract_of  IncDec) initial_storage 0mutez in
  let contr = Test.to_contract taddr in
  let a_ = Test.nth_bootstrap_account 0 in
  let b_ = Test.nth_bootstrap_account 1 in
  let c_ = Test.nth_bootstrap_account 2 in
  let d_ = Test.nth_bootstrap_account 3 in
  let op0 = Test.Incremental.transfer contr (Increment 0) 0mutez a_ in
  let op1 = Test.Incremental.transfer contr (Increment 1) 0mutez b_ in
  let op2 = Test.Incremental.transfer contr (Increment 2) 0mutez c_ in
  let op3 = Test.Incremental.transfer contr (Increment 3) 0mutez d_ in
  let ops : Test.Incremental.operation list = [op0; op1; op2; op3] in
  let _n = Test.Incremental.bake_exn ops in
  let () = Test.log (Test.get_time ()) in
  let () = Test.log _n in
  assert (Test.get_storage taddr = initial_storage + 6)

let test_separate =
  let () = Test.println "Separate" in
  let () = Test.reset_state 10n ([] : tez list) in
  let () = Test.log (Test.get_time ()) in
  let (taddr, _, _) =
    Test.originate_module (contract_of  IncDec) initial_storage 0mutez in
  let contr = Test.to_contract taddr in
  let _ = Test.transfer_to_contract_exn contr (Increment 0) 0mutez in
  let _ = Test.transfer_to_contract_exn contr (Increment 1) 0mutez in
  let _ = Test.transfer_to_contract_exn contr (Increment 2) 0mutez in
  let _ = Test.transfer_to_contract_exn contr (Increment 3) 0mutez in
  let () = Test.log (Test.get_time ()) in
  assert (Test.get_storage taddr = initial_storage + 6)

let test_increment_gas =
  let () = Test.println "Increment (gas)" in
  let () = Test.reset_state 10n ([] : tez list) in
  let (taddr, _, _) =
    Test.originate_module (contract_of  IncDec) initial_storage 0mutez in
  let contr = Test.to_contract taddr in
  let a_ = Test.nth_bootstrap_account 0 in
  let b_ = Test.nth_bootstrap_account 1 in
  let op0 = Test.Incremental.transfer contr (Increment 0) 0mutez a_ in
  let op1 = Test.Incremental.transfer contr (Increment 1) 0mutez b_ in
  let ops : Test.Incremental.operation list = [op0; op1] in
  let () = Test.log (Test.Incremental.bake ops) in
  assert (Test.get_storage taddr = initial_storage + 1)

let test_increment_gas =
  let () = Test.println "Separate (gas)" in
  let () = Test.reset_state 10n ([] : tez list) in
  let (taddr, _, _) =
    Test.originate_module (contract_of  IncDec) initial_storage 0mutez in
  let contr = Test.to_contract taddr in
  let () = Test.log (Test.transfer_to_contract contr (Increment 0) 0mutez) in
  let () = Test.log (Test.transfer_to_contract contr (Increment 1) 0mutez) in
  assert (Test.get_storage taddr = initial_storage + 1)

let test_increment_fail =
  let () = Test.println "Increment fail" in
  let () = Test.reset_state 10n ([] : tez list) in
  let (taddr, _, _) =
    Test.originate_module (contract_of  IncDec) initial_storage 0mutez in
  let contr = Test.to_contract taddr in
  let a_ = Test.nth_bootstrap_account 0 in
  let b_ = Test.nth_bootstrap_account 1 in
  let c_ = Test.nth_bootstrap_account 2 in
  let d_ = Test.nth_bootstrap_account 3 in
  let op0 = Test.Incremental.transfer contr (Increment 0) 0mutez a_ in
  let op1 = Test.Incremental.transfer contr (Increment 1) 0mutez b_ in
  let op2 = Test.Incremental.transfer contr (Brk ()) 0mutez c_ in
  let op3 = Test.Incremental.transfer contr (Increment 3) 0mutez d_ in
  let ops : Test.Incremental.operation list = [op0; op1; op2; op3] in
  let () = Test.log (Test.Incremental.bake ops) in
  assert (Test.get_storage taddr = initial_storage)