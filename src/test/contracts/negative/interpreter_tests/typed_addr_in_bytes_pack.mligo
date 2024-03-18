let originate_record () =
    let orig = Test.originate_from_file "./unit_contract.mligo" () 0mav in
    let addr = Test.to_address orig.addr in
    let contr = Test.to_contract orig.addr in
    {
        contr = contr ;
        addr  = addr  ;
        taddr = (orig.addr :(unit, unit) typed_address) ;
    }

let test =
    let r = originate_record () in
    let packed = Bytes.pack (fun() -> 
        match (Mavryk.get_entrypoint_opt "%transfer" r.addr : unit contract option) with
          Some(c) -> let op = Mavryk.transaction () 0mumav c in [op]
        | None ->  ([] : operation list)
    ) in
    let () = Test.log(packed) in
    ()