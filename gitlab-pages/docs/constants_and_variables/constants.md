---
id: constants
title: Constants
---

import Syntax from '@theme/Syntax';

Constants are defined by assigning the value of an expression to a
variable (that is, a value name). They are immutable by design, which
means that their values cannot be reassigned. Put in another way, they
can be assigned once, at their declaration. When defining a constant
you can also ascribe a type to it.

<Syntax syntax="cameligo">

Constant declarations are introduced by the keyword `let`, like so:

```cameligo group=constants
let a = 1
let b : int = a // Type ascription (a.k.a. annotation)
```

</Syntax>

<Syntax syntax="jsligo">

Constant declarations are introduced by the keyword `const`, like so:

```jsligo group=constants
const a = 1;
const b : int = a; // Type ascription (a.k.a. annotation)
```

Note that variables cannot be redefined in the same block scope:

```jsligo skip
const c = do {
  const x = 1;
  const x = 2; // Yields an error
};
```

However, the following does work:

```jsligo group=constants
const d = do {
  const x = 1;
  {
    const x = 2; // Does not yield an error: another block
    return x;
  }
};
```

</Syntax>
