let main ((_, _) : (unit * unit)) : operation list * unit =
  let v = (Mavryk.get_entrypoint_opt
           "foo"
           ("mv198hRfuJgRidm2CGP9UqehFCneSz3a2TZm" : address) : unit contract option) in
  let u : unit = match v with
          | None -> failwith "None"
          | Some _ -> failwith "Some" in
  ([] : operation list), u
