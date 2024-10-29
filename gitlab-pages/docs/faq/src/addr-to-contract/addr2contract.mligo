let test =
  let addr : address = "mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" in
  let taddr : (unit, unit) typed_address = Test.cast_address addr in
  let contract : (unit) contract = Test.to_contract taddr in
  contract