---
id: ithaca
title: Ithaca
description: Ithaca changes
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

## API

### New primitives / behaviour

#### Subtraction operator `-`

The behaviour of the subtraction operator `-` on values of type `tez` has been changed in protocol Ithaca.

Subtracting values of type `tez` yeilds on optional value. (This emits
the `SUB_MUMAV` Michelson instruction)

<Syntax syntax="pascaligo">

```pascaligo group=b

const d : option (tez) = 5mumav - 1mumav (* Some (4mumav) *)

const e : option (tez) = 1mumav - 5mumav (* None *)

```

</Syntax>

<Syntax syntax="cameligo">

```cameligo group=b
let d : tez option = 5mumav - 1mumav (* Some (4mumav) *)
let e : tez option = 1mumav - 5mumav (* None *)

```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
let d : option<tez> = (5 as mumav) - (1 as mumav); /* Some (4mumav) */
let e : option<tez> = (1 as mumav) - (5 as mumav); /* None */

```

</Syntax>


#### Option Module

<SyntaxTitle syntax="pascaligo">
val map : ('a -> 'b) -> option ('a) -> option ('b)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val map : ('a -> 'b) -> 'a option -> 'b option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let map: (f : ((item: 'a) => 'b), value : option&lt;'a&gt;) => option&lt;'b&gt;
</SyntaxTitle>

Applies the mapper function to the value if it is wrapped in the `Some` constructor.

If the value is `None` the function is not executed/applied.


## Feature no longer supported


### Test

<SyntaxTitle syntax="pascaligo">
val set_now : timestamp -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val set_now : timestamp -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let set_now = (now: timestamp) => unit
</SyntaxTitle>

We no longer support `Test.set_now` in LIGO `0.38.0` onwards, this is because
the underlying functions used by the LIGO Testing framework do not support
setting exact timestamps.
