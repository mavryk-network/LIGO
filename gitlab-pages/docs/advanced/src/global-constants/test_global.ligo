module C is {
  type storage is int
  type parameter is unit

  function f (const x : int) : int is x * 3 + 2

  const ct : string = Test.register_constant (Test.eval (f))

  [@entry]
  function main (const _p : parameter; const store : storage) : list (operation) * storage is
    block {
      const cf : int -> int = Mavryk.constant (ct)
    } with ((nil : list (operation)), cf (store))
}

const test_it = block {
  const orig = Test.originate (contract_of C, 1, 0mumav);
  const _r = Test.transfer_exn (orig.addr, Main (unit), 0mumav);
  const s = Test.get_storage (orig.addr);
} with assert (s = 5)