---
id: comparing
title: Comparing
---

import Syntax from '@theme/Syntax';

You can compare timestamps using the same comparison operators
applying to numerical value.

<Syntax syntax="cameligo">

```cameligo group=comparing
let today : timestamp = Mavryk.get_now ()
let one_day : int = 86400
let in_24_hrs : timestamp = today - one_day
let not_tomorrow : bool = (Mavryk.get_now () = in_24_hrs)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=comparing
const today: timestamp = Mavryk.get_now();
const one_day: int = 86400;
const in_24_hrs: timestamp = today - one_day;
const not_tomorrow: bool = (Mavryk.get_now() == in_24_hrs);
```

</Syntax>
