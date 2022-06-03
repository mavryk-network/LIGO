let test_dry_run =
  (* TODO handle now *)
  let balance : tez = match CLI_PARAMETERS.balance with Some x -> x | None -> 0tez in
  let amount : tez = match CLI_PARAMETERS.amount with Some x -> x | None -> 0tez in
  let (c, _, _) =
    Test.originate_from_file CLI_PARAMETERS.filename CLI_PARAMETERS.entrypoint ([] : string list) (Test.eval CLI_PARAMETERS.storage) balance
  in
  let tx = Test.transfer c (Test.eval CLI_PARAMETERS.parameter) amount in
  let new_storage = Test.get_storage_of_address c in
  new_storage, tx
