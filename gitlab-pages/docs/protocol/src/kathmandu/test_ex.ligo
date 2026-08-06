module C is {
  [@entry] function main (const p : int * int; const _u : unit) : list (operation) * unit is
    block {
      const op1 : operation = Mavryk.emit ("%foo", p);
      const op2 : operation = Mavryk.emit ("%foo", p.0);
    } with (list [op1; op2], unit)
}

const test_foo =
  block {
    const orig = Test.originate (contract_of C, unit, 0mav);
    const _r = Test.transfer_exn (orig.addr, Main (1, 2), 0mav);
    const r1 : list (int * int) = Test.get_last_events_from (orig.addr, "foo");
    const r2 : list (int) = Test.get_last_events_from (orig.addr, "foo");
  } with (r1, r2)