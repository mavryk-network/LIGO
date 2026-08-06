---
id: cameligo-ocaml-syntax-diff
title: What are the differences between syntaxes of CameLIGO and OCaml ?
---

Most of the CameLIGO syntax follows the OCaml syntax, however, there are a few syntactic shortcuts available in one but not the other.

### Consecutive '-' operators

In OCaml, you can do :

```ocaml
let y = - -1 (* In OCaml *)
```
But this has been forbidden in CameLIGO, you have to add parentheses instead:

```cameligo
let y = -(-1) (* In CameLIGO *)
```

PascaLIGO also requires the parentheses for consecutive `-` operators:

```pascaligo
const y : int = -(-1) // In PascaLIGO
```

### Unary '+' operator

This is possible in OCaml but not CameLIGO :

```ocaml
let x = +1 (* In OCaml *)
```

### 'type in' statements

In CameLIGO, you can declare types locally to an expression.
For example, here is a function returning a list of integers :

```cameligo
(* In CameLIGO *)
let res = 
  type t = int list in
  let x : t = [42] in
  43 :: x
```

PascaLIGO does not have an inline `type ... in` form; the closest equivalent is a plain type declaration in the same scope:

```pascaligo
type t is list (int)
const x : t = list [42]
const res : t = 43 # x
```

### Entry point declarations

Although it is possible to execute code at the top-level using e.g. `ligo run test contract.mligo`,
a CameLIGO program will need one or more entry points in order to be published as an on-chain contract.

These are "main" functions which can be invoked by sending a transaction to
the blockchain. An entry point must have the type
`parameter -> storage -> operation list * storage`, where the `storage` type
must be the same for all entry points in a given contract, but different
entry points will typically use different parameter types.

An entry point will take the value of the parameter passed in the transaction,
and the value of the permanent on-chain storage, and will return a list of new transactions initiated from the contract (i.e. transfers of 0 or more tokens to other contracts or to implicit account addresses), and a new value
for the on-chain storage. The next transaction sent to that contract will use
the updated storage, and so on. In order to provide a pure function that may
consult the storage without modifying it, one can use `@view` instead of `@entry`. A _view_ can be called by another contract without generating a
transaction (i.e., the call is performed synchronously, instead of returning a delayed transaction which would run after the end of this contract's execution), and the view can return any value (since it cannot produce new transactions nor an updated storage, it simply returns the desired output value).

```cameligo
type storage = string
type result = operation list * storage

[@entry] let hello (_u : unit) (_store : storage) : result =
  [], "hello"

[@entry] let big (_u : unit) (store : storage) : result =
  [], store ^ " big"

type planet = Earth | Mars | Earth2

[@view] let world (p : planet) (store : storage) : string =
  let world = match p with
    | Earth -> " pale blue dot"
    | Mars -> " pale red dot"
    | Earth2 -> failwith "backup planet not found"
  in
  store ^ " " ^ world
```

The same contract in PascaLIGO:

```pascaligo
type storage is string
type result is list (operation) * storage

[@entry] function hello (const _u : unit; const _store : storage) : result is
  ((nil : list (operation)), "hello")

[@entry] function big (const _u : unit; const store : storage) : result is
  ((nil : list (operation)), store ^ " big")

type planet is
  Earth of unit
| Mars of unit
| Earth2 of unit

[@view] function world (const p : planet; const store : storage) : string is
  block {
    const desc : string =
      case p of [
        Earth (_u)  -> " pale blue dot"
      | Mars (_u)   -> " pale red dot"
      | Earth2 (_u) -> failwith ("backup planet not found")
      ]
  } with store ^ " " ^ desc
```

### Semicolons in `begin ... end` sequences

In OCaml, the last instruction of a `begin ... end` sequence can be terminated by a semicolon `;`, but not in CameLIGO.

```cameligo group=semicolons
(* In CameLIGO *)
type storage = int

[@entry]
let main (_p : unit) (s : storage) : operation list * storage =
  let tests =
    begin
      assert (1 = 1);
      assert (2 = 2) // no semicolon here
    end
  in
  [], s
```

PascaLIGO has an equivalent `begin ... end` block form, but it does allow a semicolon after the last instruction:

```pascaligo group=semicolons
type storage is int

[@entry]
function main (const _p : unit; const s : storage) : list (operation) * storage is
  begin
    assert (1 = 1);
    assert (2 = 2); // a trailing semicolon here is fine
  end with ((nil : list (operation)), s)
```

### Name punning

Name punning permits record assignments without repeating the right-hand side if it is the same as the record field name.
Although possible in OCaml, this is not yet avaiable in CameLIGO.

```ocaml
(* In OCaml *)
type point = {
  x : int;
  y : int;
}

let x = 24
let y = 42
  
let p_assign_without_punning : point = {x = x; y = y}
let p_assign_with_punning    : point = {x; y}  (* Unavailable in CameLIGO *)
```

<!-- updated use of entry -->