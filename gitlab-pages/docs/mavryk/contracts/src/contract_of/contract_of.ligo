type storage is int
type return is list (operation) * storage

module C is {
  [@entry] function decrement (const param : int; const storage : storage) : return is
    ((nil : list (operation)), storage - param)

  [@entry] function increment (const param : int; const storage : storage) : return is
    ((nil : list (operation)), storage + param)

  [@entry] function reset (const _u : unit; const _s : storage) : return is
    ((nil : list (operation)), 0)
}

const test_initial_storage =
  block {
    const init_storage = 42;
    const fee = 0mumav;
    const orig = Test.Next.originate (contract_of C, init_storage, fee);
    const new_storage = Test.Next.Typed_address.get_storage (orig.taddr);
  } with assert (new_storage = init_storage)