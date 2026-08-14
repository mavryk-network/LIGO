module C is {
  [@entry] function increment (const p : int; const s : int) : list (operation) * int is ((nil : list (operation)), s + p)
  [@entry] function decrement (const p : int; const s : int) : list (operation) * int is ((nil : list (operation)), s - p)
}
const test_it = block {
  const orig = Test.originate (contract_of C, 0, 0mumav);
  const _r = Test.transfer_exn (orig.addr, Increment (42), 0mumav);
  const s = Test.get_storage (orig.addr);
} with assert (s = 42)