---
id: concatenation
title: Concatenation
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

Strings can be concatenated using the `^` operator, as in OCaml:

```cameligo group=concatenation
let name = "Alice"
let greeting = "Hello"
let full_greeting = greeting ^ " " ^ name
```

</Syntax>

<Syntax syntax="jsligo">

Strings can be concatenated using the overloaded `+` operator, like
so:

```jsligo group=concatenation
const name = "Alice";
const greeting = "Hello";
const full_greeting = greeting + " " + name;
```

</Syntax>
