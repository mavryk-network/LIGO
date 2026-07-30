[@entry]
let main (_ : unit) (_ : unit) : operation list * unit =
  let dst : (unit contract) option = Mavryk.get_entrypoint_opt "%Upper" (Mavryk.get_sender ()) in
  match dst with
  | None -> failwith "lol"
  | Some dst ->
    let op : operation = Mavryk.transaction () 0mumav dst in
    ([op], ())
