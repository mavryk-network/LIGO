module C = struct
  type storage = int
  type parameter = unit
  type return = operation list * storage

  let f (x : int) = x * 3 + 2

  let ct = Test.register_constant (Test.eval f)

  [@entry]
  let main (() : parameter) (store : storage) : return =
    [], (Mavryk.constant ct store)
end

let test =
  let orig = Test.originate (contract_of C) 1 0mav in
  let _ = Test.transfer_exn orig.addr (Main ()) 0mav in
  assert (Test.get_storage orig.addr = 5)