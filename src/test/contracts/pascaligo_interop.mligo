(* Cross-syntax interop: a CameLIGO test imports a PascaLIGO module and originates it
   via `contract_of`, proving all three syntaxes share one core AST. *)
#import "pascaligo_lib.ligo" "P"

let test =
  let orig = Test.originate (contract_of P.C) 0 0mav in
  let _ = Test.transfer_exn orig.addr (Add 5) 0mav in
  assert (Test.get_storage orig.addr = 5)
