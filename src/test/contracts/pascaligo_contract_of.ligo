// PascaLIGO contract_of + parameter_of interop (run with `ligo run test`).
// Test.originate(contract_of C, ...) exercises the packaged-contract path, and
// `parameter_of C` names the module's synthesized entrypoint parameter type.

module C is {
  type storage is int
  [@entry] function add (const n : int; const s : storage) : list(operation) * storage is
    ((nil : list(operation)), s + n)
}

type c_param is parameter_of C

const test_add =
  block {
    const orig = Test.originate(contract_of C, 0, 0mumav);
    const _r = Test.transfer_exn(orig.addr, Add(5), 0mumav);
    const s = Test.get_storage(orig.addr);
  } with assert (s = 5)
