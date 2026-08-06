---
id: nesting
title: Nesting
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

Modules can be nested, which means that we can define a module inside
another module. As an illustration, let us define a variant of `Euro`
in which the constants are all grouped inside using a sub-module.

```cameligo group=module_nesting
module Euro =
  struct
    type t = nat

    let add (a, b : t * t) : t = a + b

    module Coin =
      struct
        let one : t = 1n
        let two : t = 2n
      end
  end
```

To access nested modules we simply apply the selection operator more
than once:

```cameligo group=module_nesting
type storage = Euro.t

let increment (s : storage) : storage =
  Euro.add (s, Euro.Coin.one)
```

</Syntax>


<Syntax syntax="jsligo">

Namespaces can be nested, which means that we can define a namespace
inside another namespace. As an illustration, let us define a variant
of `Euro` in which the constants are all grouped inside using a
sub-namespace.

```jsligo group=namespace_nesting
namespace Euro {
  export type t = nat;

  export let add = (a: t, b: t): t => a + b;

  export namespace Coin {
    export let one: t = 1n;
    export let two: t = 2n;
  };
};
```

To access nested namespaces we simply apply the selection operator
more than once:

```jsligo group=namespace_nesting
type storage = Euro.t;

const increment = (s: storage) : storage =>
  Euro.add (s, Euro.Coin.one);
```

Note that the sub-namespace `Coin` had to be prefixed by the keyword
`export` to enable access to its contents.

</Syntax>


<Syntax syntax="pascaligo">

Modules can be nested, which means that we can define a module inside
another module. As an illustration, let us define a variant of `Euro`
in which the constants are all grouped inside using a sub-module.

```pascaligo group=module_nesting
module Euro is {
  type t is nat

  function add (const a : t; const b : t) : t is a + b

  module Coin is {
    const one : t = 1n
    const two : t = 2n
  }
}
```

To access nested modules we simply apply the selection operator more
than once:

```pascaligo group=module_nesting
type storage is Euro.t

function increment (const s : storage) : storage is
  Euro.add (s, Euro.Coin.one)
```

</Syntax>
