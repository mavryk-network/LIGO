module C is {
  type storage is int

  [@entry] function increment (const action : int; const store : storage) : list (operation) * storage is
    ((nil : list (operation)), store + action)

  [@entry] function decrement (const action : int; const store : storage) : list (operation) * storage is
    ((nil : list (operation)), store - action)

  [@view] function get_storage (const must_be_positive : bool; const storage : int) : int is
    if must_be_positive and storage < 0 then
      failwith ("Negative value in storage")
    else
      storage
}

const test_it = block {
  const initial_storage = 42;
  const orig = Test.originate (contract_of C, initial_storage, 0mav);
  const p : parameter_of C = Increment (1);
  const _r = Test.transfer_exn (orig.addr, p, 1mumav);
} with assert (Test.get_storage (orig.addr) = initial_storage + 1)