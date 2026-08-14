---
id: type-annotations
title: Type Annotations
---

import Syntax from '@theme/Syntax';

## Annotations

In certain cases, the type of an expression cannot be properly
inferred by the compiler. In order to help the type checker, you can
annotate an expression with its desired type. Here is an example:

<Syntax syntax="cameligo">

```cameligo group=d
type parameter = Back | Claim | Withdraw

type storage = {
  owner    : address;
  goal     : mav;
  deadline : timestamp;
  backers  : (address, mav) map;
  funded   : bool
}

[@entry]
let back (param : unit) (store : storage) : operation list * storage = (* Annotation *)
  if Mavryk.get_now () > store.deadline then failwith "Deadline passed."
  else
    match Map.find_opt (Mavryk.get_sender ()) store.backers with
      None ->
        let backers = Map.update (Mavryk.get_sender ()) (Some (Mavryk.get_amount ())) store.backers
        in [], {store with backers=backers}
    | Some (x) -> [], store
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=d
type parameter =
  ["Back"]
| ["Claim"]
| ["Withdraw"];

type storage = {
  owner    : address,
  goal     : mav,
  deadline : timestamp,
  backers  : map<address, mav>,
  funded   : bool
};

@entry
const back = (param : unit, store : storage) : [list<operation>, storage] => { // Annotation
  if (Mavryk.get_now() > store.deadline) {
    return failwith ("Deadline passed.");
  }
  else {
    return match(Map.find_opt (Mavryk.get_sender(), store.backers)) {
      when(None()): do {
        let backers = Map.update(Mavryk.get_sender(), Some(Mavryk.get_amount()), store.backers);
        return [[], {...store, backers:backers}];
      };
      when(Some(x)): [[], store]
    }
  };
};
```

</Syntax>

<Syntax syntax="pascaligo">

```pascaligo group=d
type parameter is
  Back of unit
| Claim of unit
| Withdraw of unit

type storage is record [
  owner    : address;
  goal     : mav;
  deadline : timestamp;
  backers  : map (address, mav);
  funded   : bool
]

[@entry]
function back (const _param : unit; const store : storage) : list (operation) * storage is
  if Mavryk.get_now () > store.deadline then failwith ("Deadline passed.")
  else
    case Map.find_opt (Mavryk.get_sender (), store.backers) of [
      None -> block {
        const backers = Map.update (Mavryk.get_sender (), Some (Mavryk.get_amount ()), store.backers)
      } with ((nil : list (operation)), store with record [backers = backers])
    | Some (_x) -> ((nil : list (operation)), store)
    ]
```

</Syntax>

<!-- updated use of entry -->
