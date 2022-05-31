let test_dry_run =
  let balance : tez = match CLI_PARAMETERS.balance with Some x -> x | None -> 0tez in
  let amount :tez = match CLI_PARAMETERS.amount with Some x -> x | None -> 0tez in
  let () = Test.reset_state 3n [ 4000000tez ; 4000000tez ; balance] in
  let (c, _, _) =
    Test.originate_from_file
      CLI_PARAMETERS.filename
      CLI_PARAMETERS.entrypoint
      ([] : string list)
      (Test.eval CLI_PARAMETERS.storage)
      amount
  in
  let gas_consumption =
    Test.transfer_exn
      c
      (Test.eval CLI_PARAMETERS.parameter)
      0tez
  in
  { gas_consumption = gas_consumption ; returned_storage = Test.get_storage_of_address c }
