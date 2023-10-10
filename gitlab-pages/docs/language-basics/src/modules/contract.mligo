module C = struct
  [@entry] let increment (p : int) (s : int) : operation list * int = [], s + p
  [@entry] let decrement (p : int) (s : int) : operation list * int = [], s - p
end
let test =
  let orig = Test.originate (contract_of C) 0 0tez in
  let _ = Test.transfer_exn orig.addr (Increment 42) 0tez in
  assert (42 = Test.get_storage orig.addr)