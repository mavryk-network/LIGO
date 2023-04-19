---
id: modules
title: Modules
---

import Syntax from '@theme/Syntax';

<Syntax syntax="jsligo">

> Note that in JsLIGO modules are called `namespaces`.

</Syntax>

Modules are a programming language construction that allows us to
package related definitions together. A canonical example of a module
is a data type and associated operations over it (e.g. stacks or
queues). The rest of the program can access these definitions in a
regular and abstract way, providing maintainability, reusability and
safety.

For a concrete example, we could create a module that packages a type
that represents amounts in a particular currency together with
functions that manipulate these amounts: constants, addition,
subtraction, etc. A piece of code that uses this module can be
agnostic concerning how the type is actually represented inside the
module: it's abstract.

## Declaring Modules

<Syntax syntax="pascaligo">

Modules are introduced using the `module` keyword. For example, the
following code defines a module `EURO` that packages together a type,
called `t`, together with an operation `add` that sums two values of
the given currency, as well as constants for zero and one.

```pascaligo group=EURO
module EURO is {
  type t is nat
  function add (const a : t; const b : t) : t is a + b
  const zero : t = 0n
  const one : t = 1n
}
```

</Syntax>
<Syntax syntax="cameligo">

Modules are introduced using the `module` keyword. For example, the
following code defines a module `EURO` that packages together a type,
called `t`, together with an operation `add` that sums two values of
the given currency, as well as constants for zero and one.

```cameligo group=EURO
module EURO =
  struct
    type t = nat
    let add (a , b : t * t) : t = a + b
    let zero : t = 0n
    let one : t = 1n
  end
```

As we can see, in CameLIGO we also use a `struct ... end` block to
group together the definitions made in the module.

</Syntax>

<Syntax syntax="jsligo">

Modules are introduced using the `namespace` keyword. For example, the
following code defines a module `EURO` that packages together a type,
called `t`, together with an operation `add` that sums two values of
the given currency, as well as constants for zero and one.

```jsligo group=EURO
namespace EURO {
  export type t = nat;
  export let add = (a: t, b: t): t => a + b;
  export let zero: t = 0 as nat;
  export let one: t = 1 as nat
}
```

In this example you will also notice the `export` keyword. A statement within a
module can be accessed from outside the module if it is exported.

</Syntax>

## Using Modules

We can access a module's components by using the `.` operator. Let's
suppose that our storage keeps a value in euros using the previously
defined module `EURO`. Then, we can write a `main` entry point that
increments the storage value each time it is called.

<Syntax syntax="pascaligo">

```pascaligo group=EURO
type storage is EURO.t

function main (const action : unit; const store : storage) : (list (operation)) * storage is
 (nil, EURO.add (store, EURO.one))
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=EURO
type storage = EURO.t

let main (action, store : unit * storage) : operation list * storage =
 ([], EURO.add(store, EURO.one))
```
</Syntax>

<Syntax syntax="jsligo">

```jsligo group=EURO
type storage = EURO.t;

let main = (action: unit, store: storage): [list<operation>, storage] =>
  [list([]), EURO.add(store, EURO.one)];
```

</Syntax>

In principle, we could change the implementation of `EURO`, without
having to change the `storage` type or the function `main`. For
example, if we decide later that we should support manipulating
negative values, we could change `EURO` as follows:

<Syntax syntax="pascaligo">

```pascaligo group=EURO2
module EURO is {
  type t is int
  function add (const a : t; const b : t) : t is a + b
  const zero : t = 0
  const one : t = 1
}
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=EURO2
module EURO = struct
    type t = int
    let add (a , b : t * t) : t = a + b
    let zero : t = 0
    let one : t = 1
end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=EURO2
namespace EURO {
  export type t = int;
  export let add = (a: t, b: t): t => a + b;
  export let zero: t = 0;
  export let one: t = 1;
}
```

</Syntax>

Notice that the code in `main` still works, and no change is
needed. Abstraction accomplished!

> ⚠️ Please note that code using the module `EURO` might still break
> the abstraction if it directly uses the underlying representation of
> `EURO.t`. Client code should always try to respect the interface
> provided by the module, and not make assumptions on its current
> underlying representation (e.g. `EURO.t` is an alias of `nat`).

## Nested Modules: Sub-Modules

Modules can be nested, which means that we can define a module inside
another module. Let's see how that works, and define a variant of
`EURO` in which the constants are all grouped inside using a sub-module.

<Syntax syntax="pascaligo">

```pascaligo group=EURO3
module EURO is {
  type t is nat

  function add (const a : t; const b : t) : t is a + b

  module CONST is {
    const zero : t = 0n
    const one : t = 1n
  }
}
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=EURO3
module EURO =
  struct
    type t = nat

    let add (a, b : t * t) : t = a + b

    module CONST =
      struct
        let zero : t = 0n
        let one : t = 1n
      end
  end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=EURO3
namespace EURO {
  export type t = nat;

  export let add = (a: t, b: t): t => a + b;

  export namespace CONST {
    export let zero: t = 0 as nat;
    export let one: t = 1 as nat;
  };
};
```

</Syntax>

To access nested modules we simply apply the accessor operator more
than once:

<Syntax syntax="pascaligo">

```pascaligo group=EURO3
type storage is EURO.t

function main (const action : unit; const store : storage) : (list (operation)) * storage is
 (nil, EURO.add (store, EURO.CONST.one))
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=EURO3
type storage = EURO.t

let main (action, store : unit * storage) : operation list * storage =
 ([], EURO.add(store, EURO.CONST.one))
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=EURO3
type storage = EURO.t;

let main = (action: unit, store: storage) : [list<operation>, storage] =>
 [list([]), EURO.add(store, EURO.CONST.one)]
```

</Syntax>

## Modules and Imports: Build System

Modules also allow us to separate our code in different files: when we
import a file, we obtain a module encapsulating all the definitions in
it. This will become very handy for organising large contracts, as we
can divide it into different files, and the module system keeps the naming
space clean.

Generally, we will take a set of definitions that can be naturally
grouped by functionality, and put them together in a separate
file.

<Syntax syntax="pascaligo">

For example, in PascaLIGO, we can create a file `imported.ligo`:

```pascaligo group=imported
type t is nat

function add (const a : t; const b : t) : t is a + b

const zero : t = 0n
const one : t = 1n
```

</Syntax>
<Syntax syntax="cameligo">

For example, in CameLIGO, we can create a file `imported.mligo`:

```cameligo group=imported
type t = nat

let add (a , b : t * t) : t = a + b

let zero : t = 0n
let one : t = 1n
```

</Syntax>

<Syntax syntax="jsligo">

For example, in JsLIGO, we can create a file `imported.jsligo`:

```jsligo group=imported
export type t = nat;

export const add = (a: t, b: t): t => a + b;

export const zero: t = 0 as nat;
export const one: t = 1 as nat;
```

</Syntax>

<Syntax syntax="pascaligo">

Later, in another file, we can import `imported.ligo` as a module, and
use its definitions. For example, we could create a `importer.ligo`
that imports all definitions from `imported.ligo` as the module
`EURO`:

```pascaligo
#import "./gitlab-pages/docs/language-basics/src/modules/imported.ligo" "EURO"

type storage is EURO.t

function main (const action : unit; const store : storage) : (list (operation)) * storage is
 (nil, EURO.add (store, EURO.one))
```

</Syntax>
<Syntax syntax="cameligo">

Later, in another file, we can import `imported.mligo` as a module, and
use its definitions. For example, we could create a `importer.mligo`
that imports all definitions from `imported.mligo` as the module
`EURO`:

```cameligo
#import "./gitlab-pages/docs/language-basics/src/modules/imported.mligo" "EURO"

type storage = EURO.t

let main (action, store : unit * storage) : operation list * storage =
 ([], EURO.add(store, EURO.one))
```

</Syntax>

<Syntax syntax="jsligo">

Later, in another file, we can import `imported.jsligo` as a module, and
use its definitions. For example, we could create a `importer.jsligo`
that imports all definitions from `imported.jsligo` as the module
`EURO`:

```jsligo
#import "./gitlab-pages/docs/language-basics/src/modules/imported.jsligo" "EURO"

type storage = EURO.t;

const main = (_action: unit, store: storage): [list<operation>, storage] =>
  [list([]), EURO.add(store, EURO.one)];
```

</Syntax>

We can compile the file that uses the `#import` statement directly,
without having to mention the imported file.

<Syntax syntax="pascaligo">

```shell
ligo compile contract gitlab-pages/docs/language-basics/src/modules/importer.ligo --entry-point main
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo compile contract gitlab-pages/docs/language-basics/src/modules/importer.mligo --entry-point main
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo compile contract gitlab-pages/docs/language-basics/src/modules/importer.jsligo --entry-point main
```

</Syntax>


## Module Aliases

LIGO supports module aliases, that is, modules that work as synonyms
to other (previously defined) modules. This feature can be useful if
we could implement a module using a previously defined one, but in the
future, we might need to change it.

<Syntax syntax="pascaligo">

```pascaligo group=EURO
module US_DOLLAR is EURO
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=EURO
module US_DOLLAR = EURO
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=EURO
import US_DOLLAR = EURO;
```

</Syntax>


## Modules as Contracts

When a module contains declarations that are tagged with the attribute
`@entry`, then a contract can be obtained from such module. All
declarations in the module tagged as `@entry` are grouped, and a
dispatcher contract is generated.

<Syntax syntax="cameligo">

```cameligo group=contract
module C = struct
  [@entry] let increment (p : int) (s : int) : operation list * int = [], s + p
  [@entry] let decrement (p : int) (s : int) : operation list * int = [], s - p
end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=contract
namespace C {
  // @entry
  const increment = (p : int, s : int) : [list<operation>, int] => [list([]), s + p];
  // @entry
  const decrement = (p : int, s : int) : [list<operation>, int] => [list([]), s - p];
};
```

</Syntax>

A module can be compiled as a contract using `-m`:

<Syntax syntax="cameligo">

```shell
ligo compile contract gitlab-pages/docs/language-basics/src/modules/contract.mligo -m C
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo compile contract gitlab-pages/docs/language-basics/src/modules/contract.jsligo -m C
```

</Syntax>

To access the contract from the module, the primitive `contract_of`
can be used. This is particularly useful for working with the testing
framework, in conjunction with the function `Test.originate_module`:

<Syntax syntax="cameligo">

```cameligo group=contract
let test =
  let ta, _, _ = Test.originate_module (contract_of C) 0 0tez in
  let c = Test.to_contract ta in
  let _ = Test.transfer_to_contract_exn c (Increment 42) 0tez in
  assert (42 = Test.get_storage ta)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=contract
const test = (() => {
  let [taddr, _, _] = Test.originate_module(contract_of(C), 0, 0 as tez);
  let contr = Test.to_contract(taddr);
  let _ = Test.transfer_to_contract_exn(contr, (Increment (42)), 1 as mutez);
  return assert(Test.get_storage(taddr) == 42);
}) ();
```

</Syntax>
