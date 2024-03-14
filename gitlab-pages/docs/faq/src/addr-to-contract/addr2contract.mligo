let test =
  let addr = ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address) in
  let taddr : (unit, unit) typed_address = Test.cast_address addr in
  let contract : (unit) contract = Test.to_contract taddr in
  contract