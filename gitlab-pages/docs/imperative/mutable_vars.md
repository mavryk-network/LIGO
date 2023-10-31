---
id: mutable_vars
title: Mutable variables
---

import Syntax from '@theme/Syntax';

LIGO features mutable variables, that is, variables whose values can
be reassigned --- contrary to constants, which can be only assigned
once.

<Syntax syntax="cameligo">

The declaration of mutable variables start with the usual keyword
`let` (as constants do), but followed by the keyword `mut`. The
initial assignment uses `=`, but subsequent assignments use `:=`, like
so:

```cameligo group=mutable_vars
let add (a, b : int * int) : int =
  let mut x = a + b in // Mutable x is assigned a + b
  let () = x := x + 1  // Reassignment of incremented x
  in x                 // The returned value is a + b + 1
```

</Syntax>

<Syntax syntax="jsligo">

The declaration of mutable variables start with the keyword `let`,
instead of `const` for constants. All assignments use the `=`
operator, like so:

```jsligo group=mutable_vars
const a = 3;
const b = 5;
const sum = do {
  let c = a; // not const!
  c = c + b;
  return c;  // sum == a + b
};
```

By contrast, here is an invalid reassignment:

```jsligo skip
const x = do {
  const a = 0; // First assignment
  a = 1; // Error when reassigning
};
```
</Syntax>
