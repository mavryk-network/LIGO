---
id: convert-address-to-contract
title: How to convert an address to a contract in LIGO testing framework ?
---

import Syntax from '@theme/Syntax';

In the context of testing framework,
if you want to convert an address to a contract,
you need to convert `address` to `typed_address` using `Test.cast_address`.

Then convert `typed_address` to `contract` using
`Test.to_contract`. For example:

<Syntax syntax="cameligo">

```cameligo test-ligo group=addr2contract
let test =
  let addr = ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address) in
  let taddr : (unit, unit) typed_address = Test.cast_address addr in
  let contract : (unit) contract = Test.to_contract taddr in
  contract
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=addr2contract
const test = do {
  const addr = ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" as address);
  const taddr : typed_address<unit,unit> = Test.cast_address(addr);
  const contract : contract<unit> = Test.to_contract(taddr);
  return contract;
};
```

</Syntax>

Check out the reference of the `Test` framework for exact signature of the functions [here](../reference/Test.md).

<!-- updated use of entry -->