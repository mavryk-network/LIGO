---
id: keywords
title: Keywords
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">
CameLIGO's keywords are the following: `begin`, `do`, `done`,
`downto`, `else`, `end`, `false`, `for`, `fun`, `if`, `in`, `include`,
`land`, `let`, `lor`, `lsl`, `lsr`, `lxor`, `match`, `mod`, `module`,
`mut`, `not`, `of`, `or`, `rec`, `sig`, `struct`, `then`, `true`,
`type`, `val`, `while`, `with`.
</Syntax>

<Syntax syntax="jsligo">
JsLIGO's keywords are the following: `break`, `case`, `const`,
`continue`, `default`, `else`, `export`, `false`, `for`, `from`, `if`,
`import`, `let`, `of`, `return`, `switch`, `true`, `while`, `as`,
`function`, `implements`, `interface`, `namespace`, `type`,
`contract_of`, `do`, `match`, `parameter_of`, `when`.
</Syntax>


## Escaped identifiers

Keywords cannot be used as variables or record fields. If you need to
use a keyword as a variable, you can prefix it with `@`, like so:

<Syntax syntax="cameligo">

```cameligo group=keywords
let @from = ("tz1fakefakefakefakefakefakefakcphLA5" : address)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=keywords
const @from = ("tz1fakefakefakefakefakefakefakcphLA5" as address)
```


Note that this convention, called *escaped identifiers*, conflicts
with that of *decorators*, as found in JavaScript. Therefore,
decorators are not considered valid escaped identifiers, e.g. `@entry`
is invalid as a variable.

</Syntax>
