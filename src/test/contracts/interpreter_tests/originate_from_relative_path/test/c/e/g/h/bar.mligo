let f = "../../../../../src/contract/unit.mligo"

let originate () =
    let (c_addr,_,_) = Test.originate_from_file  f "main" ([] : string list) (Test.eval ()) 0mav in
    c_addr