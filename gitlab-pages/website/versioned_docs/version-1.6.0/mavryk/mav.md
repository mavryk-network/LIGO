---
id: mav
title: mav
---

import Syntax from '@theme/Syntax';

LIGO offers some Mavryk-specific data types. Here we list some of
them. Others have their own dedicated section.

The token unit on Mavryk is called `mav` in LIGO. There are several
ways to write literal values of type `mav`:

  * units of millionth of `mav`, using the suffix `mumav` after a
    natural number, like `10000mumav` or `0mumav`;
  * units of `mav`, using the suffix `tz` or `mav`, like `3mv` or
    `3mav`;
  * decimal amounts of `tz` or `mav`, like `12.3mv` or `12.4mav`.

> The type is `mav`, *not* `mumav` --- which is a suffix to write
> literals.

Note that large amounts, like with numerical values of type `int` and
`nat`, can be expressed using underscores to separate groups of
digits, like `1_000mumav` (one thousand mumav) or `0.000_004mav`.

### Adding

Addition in LIGO is accomplished by means of the `+` infix
operator. Some type constraints apply, for example you cannot add a
value of type `mav` to a value of type `nat`.

In the following example you can find a series of arithmetic
operations, including various numerical types. However, some bits
remain in comments as they would otherwise not compile, for example,
adding a value of type `int` to a value of type `mav` is invalid. Note
that adding an integer to a natural number produces an integer.

<Syntax syntax="cameligo">

```cameligo group=mav
let sum : mav = 5mumav + 0.000_010mav
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=mav
const sum: mav = 5mumav + 1mav;
```

</Syntax>

### Subtracting

Since subtracting two amounts could result in a negative amount,
subtraction of two `mav` amounts result in an
[optional amount](../variants/options.md), like so:

<Syntax syntax="cameligo">

```cameligo group=mav
let amount : mav option = 5mumav - 1mumav (* Some (4mumav) *)
let negative : mav option = 1mumav - 5mumav (* None *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=mav
const amount: option<mav> = 5mumav - 1mumav; /* Some (4mumav) */
const negative: option<mav> = 1mumav - 5mumav; /* None */
```

</Syntax>

### Multiplying

You can multiply `nat` and `mav` values:

<Syntax syntax="cameligo">

```cameligo group=mav
let mult : mav = 5n * 5mumav
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=mav
const mult: mav = 5n * 5mumav;
```

</Syntax>

### Dividing

The division of two `mav` values results into a `nat`.

<Syntax syntax="cameligo">

```cameligo group=mav
let div : nat = 10mumav / 3mumav
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=mav
const div: nat = 10mumav / 3mumav;
```

</Syntax>

## Euclidean Division

<Syntax syntax="cameligo">

For cases when you need both the quotient and the remainder, LIGO
provides the `ediv` operation. `ediv x y` returns `Some (quotient,
remainder)`, unless `y` is zero, in which case it returns `None`. The
function `ediv` is overloaded to accept mav, beyond all the
combinations of natural and integer numbers:

```cameligo group=mav_euclidean
// Some (7, 2mumav)
let ediv1 : (nat * mav) option = ediv 37mumav 5mumav

// Some (7mumav, 2mumav)
let ediv2 : (mav * mav) option = ediv 37mumav 5n
```

</Syntax>

<Syntax syntax="jsligo">

For cases when you need both the quotient and the remainder, LIGO
provides the `ediv` operation. `ediv(x,y)` returns `Some (quotient,
remainder)`, unless `y` is zero, in which case it returns `None`. The
function `ediv` is overloaded to accept mav, beyond all the
combinations of natural and integer numbers:

```jsligo group=mav_euclidean
// Some (7, 2mumav)
const ediv1: option<[nat, mav]> = ediv(37mumav, 5mumav);

// Some (7mumav, 2mumav)
const ediv2: option<[mav, mav]> = ediv(37mumav, 5n);
```

</Syntax>
