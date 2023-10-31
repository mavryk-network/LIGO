---
id: unit
title: Unit type
---

import Syntax from '@theme/Syntax';

The type `unit` is a predefined type that contains only one value that
carries no information. It is used when no relevant information is
required or produced.

<Syntax syntax="cameligo">

The unique value of type `unit` is written `()`, like an empty tuple,
following the OCaml convention.

```cameligo group=unit
let x : unit = ()
```

</Syntax>

<Syntax syntax="jsligo">

The unique value of type `unit` is `[]`, like an empty tuple.

```jsligo group=unit
const x : unit = [];
```

</Syntax>
