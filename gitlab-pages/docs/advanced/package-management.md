---
id: package-management
title: Package management
---

Often times when working on a project there arises a need to use some common reusable piece of code, most of the time such functions are provided by the standard library of the programming language.
When it is not feasible for the standard library to provide such functions, these functions can be provided by an external library.

To fetch (download) & maintain different versions of external libraries we need a package manager.
LIGO libraries can be published to [npm](https://www.npmjs.com/) and using `ligo install` command we can fetch these ligo libraries (It internally invokes the [esy](https://esy.sh/) package manager).

There are 2 aspects to working with external packages
1. Using a package published on npm
2. Creating and publishing packages to npm

Pre-requites: 
1. Install Node.js [link](https://nodejs.org/en/download/package-manager/)
2. Install esy [link](https://esy.sh/docs/en/getting-started.html)

## Using a LIGO package published on npm

Start with empty `package.json` file

```json
{}
```

We will need the LIGO compiler binary to compile smart contracts, to get the LIGO compiler follow these [instructions](https://ligolang.org/docs/intro/installation).

Next we will use a simple dependency `ligo-list-helper` which is published on npm, to get the library use the command

```bash
ligo install ligo-list-helpers
```

Now we will write a smart contract `main.mligo` which will use the `ligo-list-herpers` library

```cameligo skip
#import "ligo-list-helpers/list.mligo" "XList"

type parameter =
  Concat  of int list
| Reverse

type storage = int list

type return = (operation) list * storage

let main (action, store : parameter * storage) : operation list * storage =
  (([]: operation list),
   (match action with
      Concat ys -> XList.concat store ys 
    | Reverse   -> XList.reverse store))

```

and we write some tests for our smart contract in `main.test.mligo`

```cameligo skip
#include "main.mligo"

let test = 
    let storage = Test.compile_value [1; 2; 3] in
    let (addr, _, _) = Test.originate_from_file "./main.mligo" "main" storage 0tez in
    let taddr : (parameter, storage) typed_address = Test.cast_address addr in
    let contr : parameter contract = Test.to_contract taddr in
    let () = Test.transfer_to_contract_exn contr Reverse 1mutez in
    assert (Test.get_storage taddr = [3; 2; 1])

```

To compile the contract to Michelson run the command

```bash
ligo compile contract main.mligo --project-root .
```

This will download the dependencies automatically, and compile the `main.mligo` file.

To test the contract using ligo's [testing framework](https://ligolang.org/docs/reference/test) run the command

```bash
ligo run test main.test.mligo --project-root .
```

If you working with an existing LIGO project, to install the dependencies, at the root of the project just run

```bash
ligo install
```

By default dependencies are installed in the `.ligo` directory at the root of the project, If you wish to change
the path where dependencies are installed use the `--cache-path` flag to specify the path e.g.

```bash
ligo install --cache-path /some/path
```

## Creating and publishing LIGO packages to npm

Since we are going to publish the library to npm, we start by creating a npm project by running 

```bash
npm init
```

Fill in the details prompted by `npm init`.
Next we are going to need the LIGO compiler binary, to get the LIGO compiler follow these [instructions](https://ligolang.org/docs/intro/installation).

We are going the write the `ligo-list-helpers` library that we used earlier.

```cameligo skip
(* LIGO library for working with lists *)

let concat (type a) (xs : a list) (ys : a list) : a list =
    let f (x,ys : (a * a list)) : a list = x :: ys in
    List.fold_right f xs ys

let reverse (type a) (xs : a list) : a list =
    let f (ys,x : (a list * a)) : a list = x :: ys in
    List.fold_left f ([] : a list) xs

```

and some tests for the library

```cameligo skip
#include "list.mligo"

let test_concat = 
    let xs = [1; 2; 3] in
    let ys = [4; 5; 6] in
    let zs = concat xs ys in
    assert (zs = [1; 2; 3; 4; 5; 6])

let test_reverse = 
    let xs = [1; 2; 3] in
    assert (reverse xs = [3; 2; 1])

```

To run the tests run the command

```bash
ligo run test list.test.mligo --project-root .
```

Now the final step is publishing the library to npm.
The steps are the same as [publishing yet another library on npm](https://docs.npmjs.com/creating-and-publishing-scoped-public-packages#publishing-scoped-public-packages)

```bash 
npm publish # this will publish the library on npm
```
