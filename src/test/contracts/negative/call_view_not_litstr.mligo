[@entry]
let main (s : string) () : operation list * unit =
  let u =
    match (Tezos.call_view
         s
         (Tezos.get_sender ())
         ("mv2fakefakefakefakefakefakefak82z7t2" : address)
       : unit option)
    with
      Some x -> x
    | None -> () in
  ([] : operation list), u
