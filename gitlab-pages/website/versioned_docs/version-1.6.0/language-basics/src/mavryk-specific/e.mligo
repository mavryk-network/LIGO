let origination : operation * address = Mavryk.create_contract
  (fun (p : nat) (s : string) -> ([], s))
  None
  3tz
  "initial_storage"