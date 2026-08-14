---
id: including
title: Including
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

When writing a new version of a module, it is often needed to add new
features, that is, new types and values, for example when implementing
the next version of a standard. This can be achieved by defining a new
module that includes the types and values of the old one, and defines
new ones.

In the following example, let us extend the `Euro` module with a 10
euro note. The inclusion of a module `M` is specified with a field
`include M`, like so:

```cameligo group=including
module Euro =
  struct
    type t = nat
    let add (a, b : t * t) : t = a + b
    let one : t = 1n
    let two : t = 2n
  end

module NewEuro =
  struct
    include Euro
    let ten : t = 10n
  end
```

</Syntax>

<Syntax syntax="jsligo">
This feature is not available in JsLIGO.
</Syntax>

<Syntax syntax="pascaligo">

PascaLIGO 0.73 does not have a module-body `include` declaration: its
grammar only allows `include` inside a `sig ... end` signature, to
combine module *types*. A new module cannot splice another module's
declarations into its own body this way, so this particular example
— extending `Euro` with a 10 euro note by including it in `NewEuro` —
is not available in PascaLIGO.

</Syntax>
