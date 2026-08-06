const test =
  block {
    const addr : address = "mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe";
    const taddr : typed_address (unit, unit) = Test.cast_address (addr);
    const contract : contract (unit) = Test.to_contract (taddr);
  } with contract