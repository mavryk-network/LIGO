let test_dry_run =
  let (c, _, _) =
    Test.originate_from_file
      CLI_PARAMETERS.filename
      CLI_PARAMETERS.entrypoint
      ([] : string list)
      (Test.eval CLI_PARAMETERS.storage)
      0tez
  in
  let gas_consumption =
    Test.transfer_exn
      c
      (Test.eval CLI_PARAMETERS.parameter)
      0tez
  in
  { gas_consumption = gas_consumption ; returned_storage = Test.get_storage_of_address c }


// let test_dry_run =
//   CLI_PARAMETERS.filename