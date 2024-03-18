[@entry]
let main () () : operation list * unit =
  let u =
    match (Mavryk.call_view
         "foo"
         (Mavryk.get_sender ())
         ("mv2fakefakefakefakefakefakefak82z7t2" : address)
       : unit option)
    with
      Some x -> x
    | None -> () in
  ([] : operation list), u
