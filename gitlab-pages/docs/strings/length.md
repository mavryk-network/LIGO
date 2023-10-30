---
id: length
title: Length
---

import Syntax from '@theme/Syntax';

The length of a string can be obtain by calling the predefined
function `String.length`.

<Syntax syntax="cameligo">

```cameligo group=length
let name = "Alice"
let length = String.length name  // length = 5
```

> Note that `String.size` is *deprecated*.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=length
const name = "Alice";
const length = String.length(name);  // length == 5
```

</Syntax>
