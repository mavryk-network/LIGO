module C = struct
  [@entry] let main (p : int*int) () =
    [Mavryk.emit "%foo" p ; Mavryk.emit "%foo" p.0],()
end

let test_foo =
  let orig = Test.Next.Originate.contract (contract_of C) () 0mav in
  let _: nat = Test.Next.Typed_address.transfer_exn orig.taddr (Main (1,2)) 0mav in
  (Test.Next.State.last_events orig.taddr "foo" : (int*int) list),(Test.Next.State.last_events orig.taddr "foo" : int list)