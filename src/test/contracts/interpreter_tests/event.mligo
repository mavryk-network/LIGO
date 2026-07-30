module C = struct
  [@entry]
  let main (p : int * int) (_ : unit) : operation list * unit =
    [Mavryk.emit "%foo" p ; Mavryk.emit "%foo" p.0],()
end

let test_foo =
  let orig = Test.originate (contract_of C) () 0mav in
  let _ = Test.transfer_exn orig.addr (Main (1,2)) 0mav in
  (Test.get_last_events_from orig.addr "foo" : (int*int) list),(Test.get_last_events_from orig.addr "foo" : int list)