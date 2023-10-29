---
id: negation
title: Negation
---

The arithmetic negation of a number is the same as subtracting that
number from zero, so the negation of a natural numbers yields an
integer:

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
