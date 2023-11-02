let main ((_, _) : unit * unit) : operation list * unit =
  let u = match (Mavryk.call_view "foo" (Mavryk.get_sender ()) ("mv198hRfuJgRidm2CGP9UqehFCneSz3a2TZm" : address) : unit option) with
    | Some x -> x
    | None -> () in
  ([] : operation list), u
