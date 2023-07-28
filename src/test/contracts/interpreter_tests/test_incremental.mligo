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

end

(* Tests for main access point *)

let initial_storage = 42

let test_increment =
  let () = Test.reset_state 10n ([] : tez list) in
  let (taddr, _, _) =
    Test.originate_module (contract_of  IncDec) initial_storage 0mutez in
  let contr = Test.to_contract taddr in
  let c_ = Test.nth_bootstrap_account 2 in
  let d_ = Test.nth_bootstrap_account 3 in
  let op1 = Test.Incremental.transfer contr (Increment 1) 1mutez c_ in
  let op2 = Test.Incremental.transfer contr (Increment 2) 1mutez d_ in
  let ops : Test.Incremental.operation list = [op1; op2] in
  let () = Test.Incremental.bake ops in
  assert (Test.get_storage taddr = initial_storage + 3)
