---
id: asserting
title: Asserting
---

import Syntax from '@theme/Syntax';

Assertions can be used to ensure a certain condition is met when
running a contract. The predefined function `assert` is used to check
whether a given a Boolean condition holds. The function `assert_some`
is used to check if an option value is not `None`. The function
`assert_some_with_error` is like `assert_some` but an error message
can be given. Whenever the assertion fails, the contract will stop and
an error will be left on the execution stack.

<Syntax syntax="cameligo">

```cameligo group=asserting
let incr_if_true (b : bool) (n : int) : int =
  let () = assert b in n+1

let incr_if_some (b : unit option) (n : int) : int =
  let () = assert_some b in n+1
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=asserting
const incr_if_true = (b: bool, n: int) : int => {
  assert(b);
  return n+1;
};

const incr_if_some = (b: option<unit>, n: int) : int => {
  assert_some(b);
  return n+1;
};
```

</Syntax>

<Syntax syntax="pascaligo">

```pascaligo group=asserting
function incr_if_true (const b : bool; const n : int) : int is
  block { assert (b) } with n + 1

function incr_if_some (const b : option (unit); const n : int) : int is
  block { assert_some (b) } with n + 1
```

</Syntax>

You can use `assert_with_error` or `assert_some_with_error` to use a
custom error message.

<Syntax syntax="cameligo">

```cameligo group=assert_with_error
let incr_if_true (b : bool) (n : int) : int =
  let () = assert_with_error b "My custom error message."
  in n+1
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=assert_with_error
const incr_if_true = (b: bool, n: int) : int => {
  assert_with_error (b, "My custom error message.");
  return n+1;
};
```

</Syntax>

<Syntax syntax="pascaligo">

```pascaligo group=assert_with_error
function incr_if_true (const b : bool; const n : int) : int is
  block { assert_with_error (b, "My custom error message.") } with n + 1
```

</Syntax>

Likewise, we can check for `None` instead of `Some` by using
`assert_none` and `assert_none_with_error`.
