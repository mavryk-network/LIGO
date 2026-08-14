---
id: cheat-sheet
title: Cheat Sheet
---

import Syntax from '@theme/Syntax';

<div className="cheatsheet">

<Syntax syntax="cameligo">

<div className="codeTable">
<div className="primitive">
Contract, view and test
</div>
<div className="example">

```cameligo group=simple_contract_with_view_and_test
module C = struct
  type storage = int

  [@entry]
  let increment (action: int) (store: storage) : operation list * storage =
    [], store + action

  [@entry]
  let decrement (action: int) (store: storage) : operation list * storage =
    [], store - action

  [@view]
  let get_storage (must_be_positive: bool) (storage: int) : int =
    if must_be_positive && storage < 0 then
      failwith "Negative value in storage"
    else
      storage
end

let testC =
    let initial_storage = 42 in
    let originated = Test.originate (contract_of C) initial_storage 0mav in
    let p : C parameter_of = Increment 1 in
    let _ = Test.transfer_exn originated.addr p 1mumav in
    assert (Test.get_storage originated.addr = initial_storage + 1)
```

</div>
<div className="primitive">Strings</div>
<div className="example">

```cameligo
let name : string = "Mavryk"
```

</div>
<div className="primitive">
Characters
</div>
<div className="example">

```cameligo
let t : string = "t"
```

</div>
<div className="primitive">
Integers
</div>
<div className="example">

```cameligo
let i : int = 42
```

</div>
<div className="primitive">
Natural numbers
</div>
<div className="example">

```cameligo
let n : nat = 7n
```

</div>
<div className="primitive">
Unit
</div>
<div className="example">

```cameligo
let u : unit = unit
```

</div>
<div className="primitive">
Boolean
</div>
<div className="example">

```cameligo
let has_drivers_license : bool = false
let adult : bool = true
```

</div>
<div className="primitive">
Boolean Logic
</div>
<div className="example">

```cameligo
let booleanLogic : bool =
    (not true) =
    false =
    (false && true) =
    (false || false)
```

</div>
<div className="primitive">
Mumav (micro mav)
</div>
<div className="example">

```cameligo
let mav : mav = 42mav
let mav : mav = 7mumav
```

</div>
<div className="primitive">
Address
</div>
<div className="example">

```cameligo
let mv1address : address =
  ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)
let kt1address : address =
  ("KT1JepfBfMSqkQyf9B1ndvURghGsSB8YCLMD" : address)
```

</div>
<div className="primitive">
String
</div>
<div className="example">

```cameligo
let my_str : string = "Hello World!"
```

</div>
<div className="primitive">
Verbatim string
</div>
<div className="example">

```cameligo
let verbatim_str : string = {|verbatim string|}
```

</div>
<div className="primitive">
Addition
</div>
<div className="example">

```cameligo
let add_int : int = 3 + 4
let add_nat : nat = 3n + 4n
```

</div>
<div className="primitive">
Multiplication & Division
</div>
<div className="example">

```cameligo
let mul_int : int = 3 * 4
let mul_nat : nat = 3n * 4n

let div_int : int = 10 / 5
let div_nat : nat = 10n / 5n
```

</div>
<div className="primitive">
Modulo
</div>
<div className="example">

```cameligo
let mod_nat : nat = 10 mod 3
```

</div>
<div className="primitive">
Tuples
</div>
<div className="example">

```cameligo
type name = string * string

let winner : name = "John", "Doe"

let firstName : string = winner.0
let lastName : string = winner.1
```

</div>
<div className="primitive">
Types
</div>
<div className="example">

```cameligo
type age = int
type name = string
```

</div>
<div className="primitive">
Include (prefer import)
</div>
<div className="example">

```cameligo skip
#include "library.mligo"
```

</div>
<div className="primitive">
Import (better)
</div>
<div className="example">

```cameligo skip
#import "library.mligo" "MyLibrary"
let foo = MyLibrary.bar
```

</div>
<div className="primitive">
Functions
</div>
<div className="example">

```cameligo
let add (a : int) (b : int) : int =
  a + b
```

</div>

<div className="primitive">
If Statement
</div>
<div className="example">

```cameligo
let can_drive (age : nat) : string =
  if age >= 16n then "yes" else "no"
```

</div>
<div className="primitive">
Options
</div>
<div className="example">

```cameligo
type middle_name = string option
let middle_name : middle_name = Some "Foo"
let middle_name : middle_name = None
```

</div>
<div className="primitive">
Variable Binding
</div>
<div className="example">

```cameligo
let age : int = 5
```

</div>
<div className="primitive">
Type Annotations
</div>
<div className="example">

```cameligo
let someAddress : address =
  ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)
```

</div>
<div className="primitive">
Variants
</div>
<div className="example">

```cameligo group=variants
type action =
  Increment of int
| Decrement of int
```

</div>
<div className="primitive">
Variant *(pattern)* matching
</div>
<div className="example">

```cameligo group=variants
let a : action = Increment 5

let result : int =
  match a with
    Increment n -> n + 1
  | Decrement n -> n - 1
```

</div>
<div className="primitive">
Records
</div>
<div className="example">

```cameligo
type person = {
  age  : int;
  name : string
}

let john : person = {
  age  = 18;
  name = "john doe"
}

let name : string = john.name
```

</div>
<div className="primitive">
Maps
</div>
<div className="example">

```cameligo
type prices = (nat, mav) map

let prices : prices =
  Map.literal [
    (10n, 60mumav);
    (50n, 30mumav);
    (100n, 10mumav);
  ]

let price : mav option = Map.find_opt 50n prices

let prices : prices = Map.update 200n (Some 5mumav) prices
```

</div>
<div className="primitive">
Contracts & Accounts
</div>
<div className="example">

```cameligo group=mavryk_specific
let destinationAddress : address =
  ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)

let contract : unit contract =
  match (Mavryk.get_contract_opt (Mavryk.get_sender ()) : unit contract option) with
    Some contract -> contract
    | None -> (failwith "no contract" : unit contract)
```

</div>
<div className="primitive">
Transactions
</div>
<div className="example">

```cameligo group=mavryk_specific

let payment : operation =
  Mavryk.transaction unit 100mumav contract

```

</div>
<div className="primitive">
Exception/Failure
</div>
<div className="example">

```cameligo
let fail (u : unit) : unit =
  failwith "a failure message"
```

</div>
<div className="primitive">
Comb layout (default)
</div>
<div className="example">

```cameligo
type animal =
[@layout comb]
| Elephant
| Dog
| Cat
```

</div>
<div className="primitive">
Tree layout
</div>
<div className="example">

```cameligo
type animal =
[@layout tree]
| Elephant
| Dog
| Cat
```

</div>
<div className="primitive">
Module definition (auto-inferred type)
</div>
<div className="example">

```cameligo
module FA0_inferred = struct
  type t = unit
  [@entry] let transfer (_ : unit) (_ : t) : operation list * t = [], ()
end
```

</div>
<div className="primitive">
Module Type
</div>
<div className="example">

```cameligo
module type FA0_SIG = sig
  type t
  [@entry] val transfer : unit -> t -> operation list * t
end
```

</div>
<div className="primitive">
Extending Module Type
</div>
<div className="example">

```cameligo
module type FA0Ext_SIG = sig
  include FA0_SIG
  [@entry] val transfer2 : unit -> t -> operation list * t
end
```

</div>
<div className="primitive">
Module definition
</div>
<div className="example">

```cameligo
module FA0 : FA0_SIG = struct
  type t = unit
  [@entry] let transfer (_ : unit) (_ : t) : operation list * t = [], ()
end
```

</div>
<div className="primitive">
Extending module definition
</div>
<div className="example">

```cameligo
module FA0Ext : FA0Ext_SIG = struct
  include FA0
  [@entry] let transfer2 (a : unit) (b : t) = transfer a b
end
```

</div>
</div>

</Syntax>

<Syntax syntax="jsligo">

<div className="codeTable">
<div className="primitive">
Contract, view and test
</div>
<div className="example">

```jsligo group=simple_contract_with_view_and_test
namespace C {
  export type storage = int;

  @entry
  const increment = (action: int, store: storage) : [list <operation>, storage] => [[], store + action];

  @entry
  const decrement = (action: int, store: storage) : [list <operation>, storage] => [[], store - action];

  @view
  const get_storage = (must_be_positive: bool, storage: int): int => {
    if (must_be_positive && storage < 0) {
      return failwith("Negative value in storage");
    } else {
      return storage;
    }
  }
};

const testC = do {
    let initial_storage = 42;
    let originated = Test.originate(contract_of(C), initial_storage, 0mav);
    let p : parameter_of C = Increment(1);
    Test.transfer_exn(originated.addr, p, 1mumav);
    return assert(Test.get_storage(originated.addr) == initial_storage + 1);
}
```

</div>
<div className="primitive">Strings</div>
<div className="example">

```jsligo
const name: string = "Mavryk";
```

</div>
<div className="primitive">
Characters
</div>
<div className="example">

```jsligo
const t: string = "t";
```

</div>
<div className="primitive">
Integers
</div>
<div className="example">

```jsligo
const i: int = 42;
```

</div>
<div className="primitive">
Natural numbers
</div>
<div className="example">

```jsligo
const n: nat = 7n;
```

</div>
<div className="primitive">
Unit
</div>
<div className="example">

```jsligo
const u: unit = unit;
```

</div>
<div className="primitive">
Boolean
</div>
<div className="example">

```jsligo
const has_drivers_license: bool = false
const adult: bool = true
```

</div>
<div className="primitive">
Boolean Logic
</div>
<div className="example">

```jsligo
const booleanLogic: bool =
    (!true) ==
    false ==
    (false && true) ==
    (false || false)
```

</div>
<div className="primitive">
Mumav (micro mav)
</div>
<div className="example">

```jsligo
const mav_amount: mav = 42mav
const mav_amount2: mav = mav_amount + 7mumav // == 42000007mumav
```

</div>
<div className="primitive">
Address
</div>
<div className="example">

```jsligo
const mv1address: address = "mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe";
const kt1address: address = "KT1JepfBfMSqkQyf9B1ndvURghGsSB8YCLMD";
```

</div>
<div className="primitive">
String
</div>
<div className="example">

```jsligo
const my_str : string = "Hello World!";
```

</div>
<div className="primitive">
Verbatim string
</div>
<div className="example">

```jsligo
const verbatim_str : string = `verbatim string`;
```

</div>
<div className="primitive">
Addition
</div>
<div className="example">

```jsligo
const add_int: int = 3 + 4;
const add_nat: nat = 3n + 4n;
```

</div>
<div className="primitive">
Multiplication & Division
</div>
<div className="example">

```jsligo
const mul_int: int = 3 * 4;
const mul_nat: nat = 3n * 4n;

const div_int: int = 10 / 5;
const div_nat: nat = 10n / 5n; // can fail (division by zero), check your inputs first.
```

</div>
<div className="primitive">
Modulo
</div>
<div className="example">

```jsligo
const mod_nat: nat = 10 % 3; // can fail (division by zero), check your inputs first.
```

</div>
<div className="primitive">
Tuples
</div>
<div className="example">

```jsligo
type name = [string, string];

const winner: name = ["John", "Doe"];

const firstName: string = winner[0];
const lastName: string = winner[1];
```

</div>
<div className="primitive">
Types
</div>
<div className="example">

```jsligo group=b
type age = int
type name = string
```

</div>
<div className="primitive">
Include (prefer import)
</div>
<div className="example">

```jsligo skip
#include "library.jsligo"
```

</div>
<div className="primitive">
Import (better)
</div>
<div className="example">

```jsligo skip
#import "library.jsligo" "MyLibrary"
const foo = MyLibrary.bar;
```

</div>
<div className="primitive">
Functions (short form)
</div>
<div className="example">

```jsligo
const add = (a: int, b: int): int =>
  a + b;
```

</div>
<div className="primitive">
Functions (long form)
</div>
<div className="example">

```jsligo group=b
const add = (a: int, b: int): int => {
  let c = a;
  let d = b;
  return c + d
};
```

</div>
<div className="primitive">
If/else Statement
</div>
<div className="example">

```jsligo
function if_statement (age : nat): string {
  if (age >= 16n) return "yes" else return "no"
}
```

</div>
<div className="primitive">
Options
</div>
<div className="example">

```jsligo
type middle_name = option<string>;
const a_middle_name : middle_name = Some("Foo");
const no_middle_name : middle_name = None();
```

</div>
<div className="primitive">
Variable Binding
</div>
<div className="example">

```jsligo
const age: int = 5
```

</div>
<div className="primitive">
Type Annotations
</div>
<div className="example">

```jsligo
const someAddress: address = "mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe";
```

</div>
<div className="primitive">
Variants (label + optional value)
</div>
<div className="example">

```jsligo group=variants
type action =
  ["Increment", int]
| ["Decrement", int]
| ["Reset"];
```

</div>
<div className="primitive">
Matching on variant cases
</div>
<div className="example">

```jsligo group=variants
let a: action = Increment(5)
const result: int = match(a) {
  when(Increment(n)): n + 1;
  when(Decrement(n)): n - 1;
  when(Reset()): 0;
}
```

</div>
<div className="primitive">
Records / Plain Old Data Objects
</div>
<div className="example">

```jsligo
type person = {
  age: int,
  name: string
}

const john : person = {
  age: 18,
  name: "john doe"
}

const name_: string = john.name
```

</div>
<div className="primitive">
Maps
</div>
<div className="example">

```jsligo
type prices = map<nat, mav>;

const prices: prices = Map.literal([
  [10n, 60mumav],
  [50n, 30mumav],
  [100n, 10mumav]
]);

const price: option<mav> = Map.find_opt(50n, prices)

const prices2: prices = Map.update(200n, Some (5mumav), prices)
```

</div>
<div className="primitive">
Contracts & Accounts
</div>
<div className="example">

```jsligo group=mavryk_specific
const destinationAddress: address = "mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe";

const contract : contract<unit> =
  match(Mavryk.get_contract_opt(Mavryk.get_sender()) as option<contract<unit>>) {
    when(Some(contract)): contract;
    when(None()): failwith("no contract or wrong contract type")
  }
```

</div>
<div className="primitive">
Transactions
</div>
<div className="example">

```jsligo group=mavryk_specific
const payment: operation =
  Mavryk.transaction(unit, 100mumav, contract);
```

</div>
<div className="primitive">
Exception/Failure
</div>
<div className="example">

```jsligo
const fail = (u: unit) : unit =>
  failwith("a failure message")
```

</div>
<div className="primitive">
Comb layout (default)
</div>
<div className="example">

```jsligo
type animal =
@layout("comb")
| ["Elephant"]
| ["Dog"]
| ["Cat"];
```

</div>
<div className="primitive">
Tree layout
</div>
<div className="example">

```jsligo
type animal =
@layout("tree")
| ["Elephant"]
| ["Dog"]
| ["Cat"];
```

</div>
<div className="primitive">
Namespace (auto-inferred type)
</div>
<div className="example">

```jsligo
namespace FA0_inferred {
  type storage = int;
  @entry const add = (s : int, k : int) : [list<operation>, int] => [[], s + k];
  @entry const extra = (s : int, k : int) : [list<operation>, int] => [[], s - k];
}
```

</div>
<div className="primitive">
Interface
</div>
<div className="example">

```jsligo
interface FA0_INTF {
  type storage;
  @entry const add : (s : int, k : storage) => [list<operation>, storage];
}
```

</div>
<div className="primitive">
Extending Interface
</div>
<div className="example">

```jsligo
interface FA0_EXT_INTF extends FA0_INTF {
  type storage;
  @entry const add : (s : int, k : storage) => [list<operation>, storage];
}
```

</div>
<div className="primitive">
Namespace impmlementing
</div>
<div className="example">

```jsligo
namespace FA0 implements FA0_INTF {
  export type storage = int;
  @entry const add = (s : int, k : int) : [list<operation>, int] => [[], s + k];
  @entry const extra = (s : int, k : int) : [list<operation>, int] => [[], s - k];
}
```

</div>
<div className="primitive">
Extending namespace
</div>
<div className="example">

Not available in JsLIGO, use CameLIGO.

</div>
</div>

</Syntax>

<Syntax syntax="pascaligo">

<div className="codeTable">
<div className="primitive">
Contract, view and test
</div>
<div className="example">

```pascaligo group=simple_contract_with_view_and_test
module C is {
  type storage is int

  [@entry] function increment (const action : int; const store : storage) : list (operation) * storage is
    ((nil : list (operation)), store + action)

  [@entry] function decrement (const action : int; const store : storage) : list (operation) * storage is
    ((nil : list (operation)), store - action)

  [@view] function get_storage (const must_be_positive : bool; const storage : int) : int is
    if must_be_positive and storage < 0 then
      failwith ("Negative value in storage")
    else
      storage
}

const test_it = block {
  const initial_storage = 42;
  const orig = Test.originate (contract_of C, initial_storage, 0mav);
  const p : parameter_of C = Increment (1);
  const _r = Test.transfer_exn (orig.addr, p, 1mumav);
} with assert (Test.get_storage (orig.addr) = initial_storage + 1)
```

</div>
<div className="primitive">Strings</div>
<div className="example">

```pascaligo
const name : string = "Mavryk"
```

</div>
<div className="primitive">
Characters
</div>
<div className="example">

```pascaligo
const t : string = "t"
```

</div>
<div className="primitive">
Integers
</div>
<div className="example">

```pascaligo
const i : int = 42
```

</div>
<div className="primitive">
Natural numbers
</div>
<div className="example">

```pascaligo
const n : nat = 7n
```

</div>
<div className="primitive">
Unit
</div>
<div className="example">

```pascaligo
const u : unit = unit
```

</div>
<div className="primitive">
Boolean
</div>
<div className="example">

```pascaligo
const has_drivers_license : bool = False
const adult : bool = True
```

</div>
<div className="primitive">
Boolean Logic
</div>
<div className="example">

```pascaligo
const booleanLogic : bool =
    (not True) =
    False =
    (False and True) =
    (False or False)
```

</div>
<div className="primitive">
Mumav (micro mav)
</div>
<div className="example">

```pascaligo
const mav_amount : mav = 42mav
const mav_amount2 : mav = mav_amount + 7mumav
```

</div>
<div className="primitive">
Address
</div>
<div className="example">

```pascaligo
const mv1address : address =
  ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)
const kt1address : address =
  ("KT1JepfBfMSqkQyf9B1ndvURghGsSB8YCLMD" : address)
```

</div>
<div className="primitive">
String
</div>
<div className="example">

```pascaligo
const my_str : string = "Hello World!"
```

</div>
<div className="primitive">
Verbatim string
</div>
<div className="example">

```pascaligo
const verbatim_str : string = {|verbatim string|}
```

</div>
<div className="primitive">
Addition
</div>
<div className="example">

```pascaligo
const add_int : int = 3 + 4
const add_nat : nat = 3n + 4n
```

</div>
<div className="primitive">
Multiplication & Division
</div>
<div className="example">

```pascaligo
const mul_int : int = 3 * 4
const mul_nat : nat = 3n * 4n

const div_int : int = 10 / 5
const div_nat : nat = 10n / 5n
```

</div>
<div className="primitive">
Modulo
</div>
<div className="example">

```pascaligo
const mod_nat : nat = 10 mod 3
```

</div>
<div className="primitive">
Tuples
</div>
<div className="example">

```pascaligo
type name is string * string

const winner : name = ("John", "Doe")

const firstName : string = winner.0
const lastName : string = winner.1
```

</div>
<div className="primitive">
Types
</div>
<div className="example">

```pascaligo
type age is int
type name is string
```

</div>
<div className="primitive">
Include (prefer import)
</div>
<div className="example">

```pascaligo skip
#include "library.ligo"
```

</div>
<div className="primitive">
Import (better)
</div>
<div className="example">

```pascaligo skip
#import "library.ligo" "MyLibrary"
const foo = MyLibrary.bar
```

</div>
<div className="primitive">
Functions
</div>
<div className="example">

```pascaligo
function add (const a : int; const b : int) : int is
  a + b
```

</div>

<div className="primitive">
If Statement
</div>
<div className="example">

```pascaligo
function can_drive (const age : nat) : string is
  if age >= 16n then "yes" else "no"
```

</div>
<div className="primitive">
Options
</div>
<div className="example">

```pascaligo
type middle_name is option (string)
const a_middle_name : middle_name = Some ("Foo")
const no_middle_name : middle_name = None
```

</div>
<div className="primitive">
Variable Binding
</div>
<div className="example">

```pascaligo
const age : int = 5
```

</div>
<div className="primitive">
Type Annotations
</div>
<div className="example">

```pascaligo
const someAddress : address =
  ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)
```

</div>
<div className="primitive">
Variants
</div>
<div className="example">

```pascaligo group=variants
type action is
  Increment of int
| Decrement of int
```

</div>
<div className="primitive">
Variant *(pattern)* matching
</div>
<div className="example">

```pascaligo group=variants
const a : action = Increment (5)

const result : int =
  case a of [
    Increment (n) -> n + 1
  | Decrement (n) -> n - 1
  ]
```

</div>
<div className="primitive">
Records
</div>
<div className="example">

```pascaligo
type person is record [
  age  : int;
  name : string
]

const john : person = record [
  age  = 18;
  name = "john doe"
]

const name : string = john.name
```

</div>
<div className="primitive">
Maps
</div>
<div className="example">

```pascaligo
type prices is map (nat, mav)

const prices : prices =
  Map.literal (list [
    (10n, 60mumav);
    (50n, 30mumav);
    (100n, 10mumav)
  ])

const price : option (mav) = Map.find_opt (50n, prices)

const prices : prices = Map.update (200n, Some (5mumav), prices)
```

</div>
<div className="primitive">
Contracts & Accounts
</div>
<div className="example">

```pascaligo group=mavryk_specific
const destinationAddress : address =
  ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)

const dest_contract : contract (unit) =
  case (Mavryk.get_contract_opt (Mavryk.get_sender ()) : option (contract (unit))) of [
    Some (c) -> c
  | None -> (failwith ("no contract") : contract (unit))
  ]
```

</div>
<div className="primitive">
Transactions
</div>
<div className="example">

```pascaligo group=mavryk_specific

const payment : operation =
  Mavryk.transaction (unit, 100mumav, dest_contract)

```

</div>
<div className="primitive">
Exception/Failure
</div>
<div className="example">

```pascaligo
function fail (const u : unit) : unit is
  failwith ("a failure message")
```

</div>
<div className="primitive">
Comb layout (default)
</div>
<div className="example">

```pascaligo
type animal is
[@layout comb]
| Elephant of unit
| Dog of unit
| Cat of unit
```

</div>
<div className="primitive">
Tree layout
</div>
<div className="example">

```pascaligo
type animal is
[@layout tree]
| Elephant of unit
| Dog of unit
| Cat of unit
```

</div>
<div className="primitive">
Module definition (auto-inferred type)
</div>
<div className="example">

```pascaligo
module FA0_inferred is {
  type t is unit
  [@entry] function transfer (const _p : unit; const _s : t) : list (operation) * t is
    ((nil : list (operation)), unit)
}
```

</div>
<div className="primitive">
Module Type
</div>
<div className="example">

```pascaligo
module type FA0_SIG is sig
  type t
  [@entry] const transfer : unit -> t -> list (operation) * t
end
```

</div>
<div className="primitive">
Extending Module Type
</div>
<div className="example">

```pascaligo
module type FA0_SIG is sig
  type t
  [@entry] const transfer : unit -> t -> list (operation) * t
end

module type FA0Ext_SIG is sig
  include FA0_SIG
  [@entry] const transfer2 : unit -> t -> list (operation) * t
end
```

</div>
<div className="primitive">
Module definition
</div>
<div className="example">

```pascaligo
module type FA0_SIG is sig
  type t
  [@entry] const transfer : unit -> t -> list (operation) * t
end

module FA0 : FA0_SIG is {
  type t is unit
  [@entry] function transfer (const _p : unit; const _s : t) : list (operation) * t is
    ((nil : list (operation)), unit)
}
```

</div>
<div className="primitive">
Extending module definition
</div>
<div className="example">

PascaLIGO has no `include` for module *values* (only for module
*types*), so a module is extended by delegating to the qualified
functions of the module it extends, instead of inlining them:

```pascaligo
module type FA0_SIG is sig
  type t
  [@entry] const transfer : unit -> t -> list (operation) * t
end

module type FA0Ext_SIG is sig
  include FA0_SIG
  [@entry] const transfer2 : unit -> t -> list (operation) * t
end

module FA0 : FA0_SIG is {
  type t is unit
  [@entry] function transfer (const _p : unit; const _s : t) : list (operation) * t is
    ((nil : list (operation)), unit)
}

module FA0Ext : FA0Ext_SIG is {
  type t is FA0.t
  [@entry] function transfer (const a : unit; const b : t) : list (operation) * t is FA0.transfer (a, b)
  [@entry] function transfer2 (const a : unit; const b : t) : list (operation) * t is FA0.transfer (a, b)
}
```

</div>
</div>

</Syntax>

</div>

<!-- updated use of entry -->
