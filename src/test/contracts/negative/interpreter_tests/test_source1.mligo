module C = struct [@entry] let main () () : operation list * unit = [], () end

let test =
  let {addr = taddr ; code = _ ; size = _} = Test.originate (contract_of C) () 0mav in
  let contr = Test.to_contract taddr in
  let addr = Mavryk.address contr in
  let () = Test.log addr in
  let () = Test.set_source addr in
  let _ = Test.originate (contract_of C) () 0mav in
  ()
