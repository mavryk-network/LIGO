---
id: types_and_literals
title: Types and Literals
---

import Syntax from '@theme/Syntax';

In LIGO, there are two types of numbers: integers and natural
numbers.

  * Integer literals are the same found in mainstream programming
    languages, for example, `10`, `-6` and `0`, but there is only one
    canonical zero: `0` (so, for instance, `-0` and `00` are invalid).

  * Natural numbers are written as digits followed by the suffix `n`,
    like so: `12n`, `0n`, and the same restriction on zero as integers
    applies: `0n` is the only way to specify the natural zero.

Contrary to integral numbers in other programming languages, numbers
in LIGO have arbitrary-precision, that is, they do not overflow or
underflow. (See also the section on Tezos-specific features for more.)

Digits of large numbers can be separated by an underscore, to increase
readability.

<Syntax syntax="cameligo">

```cameligo group=int_and_nat
// The following are integers
let zero = 0
let million = 1_000_000 // Grouping in French
let baekman = 100_0000 // Grouping in Korean

// The following are natural numbers
let zero_nat = 0n
let million_nat = 1_000_000n
let baekman_nat = 100_0000n
```

As a form of documentation, a type can be ascribed to each constant:

```cameligo group=typed_int_and_nat
let zero : int = 0
let million : int = 1_000_000
let baekman : int = 100_0000

let zero_nat : nat = 0n
let million_nat : nat = 1_000_000n
let baekman_nat : nat = 100_0000n
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=int_and_nat
// The following are integers
const zero = 0
const million = 1_000_000 // Grouping in French
const baekman = 100_0000 // Grouping in Korean

// The following are natural numbers
const zero_nat = 0n
const million_nat = 1_000_000n
const baekman_nat = 100_0000n
```

As a form of documentation, a type can be ascribed to each constant:

```jsligo group=typed_int_and_nat
const zero : int = 0
const million : int = 1_000_000
const baekman : int = 100_0000

const zero_nat : nat = 0n
const million_nat : nat = 1_000_000n
const baekman_nat : nat = 100_0000n
```

</Syntax>

## Casting

Mathematically, natural numbers are a strict subset of integers, and
arithmetic operations allow implicit mixes of natural numbers and
integers. In LIGO, this property is a case of *implicit type casting*,
which means that in any context where an integer is valid, a natural
number can occur. We will see below the related arithmetic operations
where this property applies, but we can also *explicitly cast* natural
numbers to integers (this is safe in all contexts where an integer is
valid) by calling the predefined function `int`. The inverse cast,
from `int` to `nat` is called in mathematics the _absolute value_, or
`abs` in LIGO.

<Syntax syntax="cameligo">

```cameligo group=explicit_cast
let one : int = int 1n // Explicit cast from nat to int
let two : nat = abs 2  // Explicit cast from int to nat
```
</Syntax>

<Syntax syntax="jsligo">

```jsligo group=explicit_cast
const one : int = int(1n); // Explicit cast from nat to int
const two : nat = abs(2);  // Explicit cast from int to nat
```
</Syntax>


## Addition

Addition in LIGO is accomplished by means of the `+` infix operator,
as shown in the following examples. Note that adding an integer to a
natural number produces an integer, as expected.

<Syntax syntax="cameligo">

```cameligo group=addition
let a : int = 5 + 10    // int + int yields int
let b : nat = 5n + 10n  // nat + nat yields nat
let c : int = 5n + 10   // nat + int yields int
// let error : nat = 5n + 10
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=addition
const a = 5 + 10;         // int + int yields int
const b : nat = 5n + 10n; // nat + nat yields nat
const c = 5n + 10;        // nat + int yields int
// const error : nat = 5n + 10;
```

</Syntax>

## Subtraction

Subtraction in LIGO is accomplished by means of the `+` infix
operator, as shown in the following examples. The rule when
subtracting two natural numbers is that the result is an integer
because, in general, the compiler cannot determine whether the value
of an expression is positive or zero.

<Syntax syntax="cameligo">

```cameligo group=subtraction
let a : int = 5 - 10  // int - int yields int
let b : int = 5n - 2n // nat - nat yields int
// let error : nat = 5n - 2n
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=subtraction
const a : int = 5 - 10;  // int - int yields int
const b : int = 5n - 2n; // nat - nat yields int
// const error : nat = 5n - 2n;
```

</Syntax>

## Negation

The arithmetic negation of a number is the same as subtracting that
number from zero, si negation of a natural numbers yields an integer:

<Syntax syntax="cameligo">

```cameligo group=negation
let a : int = -5  // - int yields int
let b : int = -5n // - nat yields int
// let error : nat = -5n
```
</Syntax>

<Syntax syntax="jsligo">

```jsligo group=negation
const a : int = -5;  // - int yields int
const b : int = -5n; // - nat yields int
// const error : nat = -5n;
```
</Syntax>


## Multiplication

The type rules for multiplication are the same as for the addition:

<Syntax syntax="cameligo">

```cameligo group=multiplication
let a : int = 5 * 5   // int * int yields int
let b : nat = 5n * 5n // nat * nat yields nat
let c : int = 5 * 5n  // int * nat yields int
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=multiplication
const a : int = 5 * 5;   // int * int yields int
const b : nat = 5n * 5n; // nat * nat yields nat
const c : int = 5 * 5n;  // int * nat yields int
```

</Syntax>

## Euclidean Division

Because LIGO features neither floating-point nor fixed-point
arithmetic, division is Euclidean, also called "integer division". The
predefined infix operator `/` returns the quotient.

<Syntax syntax="cameligo">

```cameligo group=division
let a : int = 10 / 3
let b : nat = 10n / 3n
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=division
const a : int = 10 / 3;
const b : nat = 10n / 3n;
```

</Syntax>

<Syntax syntax="cameligo">

The operator `mod` returns the remainder of the Euclidean division. It
is possible to obtain both the quotient and remainder together, by
means of the predefined function `ediv`: See optional values in the
section on Variants & Switches.

```cameligo group=mod
let rem1 : nat = 120  mod 9  // 3
let rem2 : nat = 120n mod 9  // 3
let rem3 : nat = 120n mod 9n // 3
let rem4 : nat = 120  mod 9n // 3
```

</Syntax>

<Syntax syntax="jsligo">

The operator `%` returns the modulus of the Euclidean division. It
is possible to obtain both the quotient and modulus together, by
means of the predefined function `ediv`: See optional values in the
section on Variants & Switches.

> The behaviour of the `%` operator in JsLIGO is different from
> JavaScript. In JsLIGO, `%` is a modulus operator and in JavaScript
> it is a remainder operator. In the case of positive numbers, they
> are the same, but not with negative numbers.

```jsligo group=mod
const rem1 : nat = 120  % 9;  // 3
const rem2 : nat = 120n % 9;  // 3
const rem3 : nat = 120n % 9n; // 3
const rem4 : nat = 120  % 9n; // 3
```

</Syntax>
