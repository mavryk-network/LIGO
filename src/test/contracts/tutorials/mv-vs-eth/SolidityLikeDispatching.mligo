// MAVRYK: restored as a real file (was a symlink into the retired version-1.6.0 doc snapshot;
// see Counter.mligo). Original mv-vs-eth tutorial contract, kept for contract_test coverage.
let main (parameter, storage : bytes * int) : operation list * int =
  if parameter = 0xbc1ecb8e
  then ([] : operation list), storage + 1
  else
    if parameter = 0x36e44653
    then ([] : operation list), storage - 1
    else (failwith "Unknown entrypoint" : operation list * int)
