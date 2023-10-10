let name : string = "Tezos"
let t : string = "t"
let i : int = 42
let n : nat = 7n
let u : unit = unit
let has_drivers_license : bool = false
let adult : bool = true
let booleanLogic : bool =
    (not true) =
    false =
    (false && true) =
    (false || false)
let tez : tez = 42tez
let tez : tez = 7mutez
let tz1address : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
let kt1address : address =
  ("KT1JepfBfMSqkQyf9B1ndvURghGsSB8YCLMD" : address)
let add_int : int = 3 + 4
let add_nat : nat = 3n + 4n
let mul_int : int = 3 * 4
let mul_nat : nat = 3n * 4n

let div_int : int = 10 / 5
let div_nat : nat = 10n / 5n
let mod_nat : nat = 10 mod 3
type name = string * string

let winner : name = "John", "Doe"

let firstName : string = winner.0
let lastName : string = winner.1
type age = int
type name = string
let can_drive (age : nat) : string =
  if age >= 16n then "yes" else "no"
type middle_name = string option
let middle_name : middle_name = Some "Foo"
let middle_name : middle_name = None
let age : int = 5
let someAddress : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
type person = {
  age  : int;
  name : string
}

let john : person = {
  age  = 18;
  name = "john doe"
}

let name : string = john.name
type prices = (nat, tez) map

let prices : prices =
  Map.literal [
    (10n, 60mutez);
    (50n, 30mutez);
    (100n, 10mutez);
  ]

let price : tez option = Map.find_opt 50n prices

let prices : prices = Map.update 200n (Some 5mutez) prices
let fail (u : unit) : unit =
  failwith "a failure message"