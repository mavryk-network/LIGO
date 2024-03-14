let test =
  let addr = ("mv1XJ6kbMgDvXvvtw8KBG2Ne2ngNHxLfuUvE" : address) in
  let taddr : (unit, unit) typed_address = Test.cast_address addr in
  let contract : (unit) contract = Test.to_contract taddr in
  contract