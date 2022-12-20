let mutate (m : michelson_program) : michelson_program list = [%external ("TEST_MUTATE_MICHELSON", m)]
let mutate_contract (m : michelson_contract) : michelson_contract list = [%external ("TEST_MUTATE_MICHELSON_CONTRACT", m)]

(*
let test_mutate =
  let f = Test.eval (fun (x : int) -> if (x > 10) then x + 1n + String.length "jeje" else 4) in
  List.iter (fun (m : michelson_program) -> Test.log (m, (Test.decompile m : int -> int) 11)) (mutate f)
*)

let test_incr (c : michelson_contract) =
  type parameter = Increment of int | Decrement of int | Reset in
  let addr = Test.originate_contract c (Test.eval 2) 0tez in
  let _ = Test.transfer_exn addr (Test.eval (Increment 1)) 0tez in
  let upd_st = Test.get_storage_of_address addr in
  (Test.eval 3 = upd_st)

let test_decr (c : michelson_contract) =
  type parameter = Increment of int | Decrement of int | Reset in
  let addr = Test.originate_contract c (Test.eval 0) 0tez in
  let _ = Test.transfer_exn addr (Test.eval (Decrement 1)) 0tez in
  let upd_st = Test.get_storage_of_address addr in
  (Test.eval (-1) = upd_st)

let test_reset (c : michelson_contract) =
  type parameter = Increment of int | Decrement of int | Reset in
  let addr = Test.originate_contract c (Test.eval 0) 0tez in
  let _ = Test.transfer_exn addr (Test.eval Reset) 0tez in
  let upd_st = Test.get_storage_of_address addr in
  (Test.eval 0 = upd_st)

let test_contract (c : michelson_contract) =
  let l = [test_incr c; test_decr c] in
  if List.fold (fun (acc, b) -> acc && b)  l true then
    Test.log c
  else
    ()

let test_mutate_contract =
  let c = Test.read_contract_from_file "incr.tz" in
  List.iter test_contract (mutate_contract c)

let test_eh =
  let c = Test.read_contract_from_file "incr.tz" in
  let tester v =
    let () = Test.log v in
    assert (false) in
  Test.log (Test.originate_contract_and_mutate_all c (Test.eval 0) 0tez tester)
