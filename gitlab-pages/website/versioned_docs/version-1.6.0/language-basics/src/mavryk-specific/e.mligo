let origination : operation * address = Mavryk.create_contract
  (fun (p : nat) (s : string) -> ([], s))
  None
  3mv
  "initial_storage"