---
id: operators
title: Operators
---

## Available Operators

> This list is non-exhaustive. More operators will be added in
> upcoming LIGO releases.

|Michelson   	|Pascaligo   	|Description |
|---	|---	|---	|
| `SENDER` | `Mavryk.get_sender` | Address that initiated the current transaction
| `SOURCE` | `Mavryk.get_source` | Address that initiated the transaction, which triggered the current transaction. (useful e.g. when there's a transaction sent by another contract)
| `AMOUNT` | `Mavryk.get_amount` | Amount of mav sent by the transaction that invoked the contract
| `NOW`    | `Mavryk.get_now`    | Timestamp of the block whose validation triggered execution of the contract, i.e. current time when the contract is run.
