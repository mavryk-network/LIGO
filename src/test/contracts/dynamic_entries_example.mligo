module My_contract = struct
  type storage =
    {
     storage : int;
     dynamic_entries
    }

  module Dynamic_entries = struct
    [@dyn_entry]
    let one () (_ : int) : operation list * int = [], 1

    [@dyn_entry]
    let tick : int ticket -> (int * int) -> operation list * (int * int) =
      fun _ x -> [], x

  end

  [@entry]
  let call_one () (s : storage) : operation list * storage =
    match Dynamic_entries.get_one s.dynamic_entries with
      Some f ->
        let op, storage = f () s.storage in
        op, {s with storage}
    | None -> failwith (-1)

  [@entry]
  let call_tick (p : int ticket) (s : storage) : operation list * storage =
    match Dynamic_entries.get_tick s.dynamic_entries with
      Some f ->
        let op, (i1, i2) = f p (s.storage, 0) in
        op, {s with storage = i1 + i2}
    | None -> failwith (-1)

  [@entry]
  let set_one () (s : storage) : operation list * storage =
    let one () (_ : int) : operation list * int = [], 11 in
    let dynamic_entries = Dynamic_entries.set_one (one, s.dynamic_entries) in
    [], {s with dynamic_entries}

end
