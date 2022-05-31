let get s = match Loaded.read s with Some x -> x | None -> failwith "Ligo_Stdlib missing (should only happen at ligo compile-time): please report to devs"
