---
id: slicing
title: Slicing
---

import Syntax from '@theme/Syntax';

Substrings can be extracted using the predefined function
`String.sub`. The first character has index 0 and the interval of
indices for the substring has inclusive bounds.

<Syntax syntax="cameligo">

```cameligo group=slicing
let name  = "Alice"
let slice = String.sub 0n 1n name  // slice = "A"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
const name = "Alice";
const slice = String.sub (0n, 1n, name); // slice == "A"
```

</Syntax>

> Notice how the offset and length of the slice are natural numbers.
