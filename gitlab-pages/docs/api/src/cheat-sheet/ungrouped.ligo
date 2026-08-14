const name : string = "Mavryk"
const t : string = "t"
const i : int = 42
const n : nat = 7n
const u : unit = unit
const has_drivers_license : bool = False
const adult : bool = True
const booleanLogic : bool =
    (not True) =
    False =
    (False and True) =
    (False or False)
const mav_amount : mav = 42mav
const mav_amount2 : mav = mav_amount + 7mumav
const mv1address : address =
  ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)
const kt1address : address =
  ("KT1JepfBfMSqkQyf9B1ndvURghGsSB8YCLMD" : address)
const my_str : string = "Hello World!"
const verbatim_str : string = {|verbatim string|}
const add_int : int = 3 + 4
const add_nat : nat = 3n + 4n
const mul_int : int = 3 * 4
const mul_nat : nat = 3n * 4n

const div_int : int = 10 / 5
const div_nat : nat = 10n / 5n
const mod_nat : nat = 10 mod 3
type name is string * string

const winner : name = ("John", "Doe")

const firstName : string = winner.0
const lastName : string = winner.1
type age is int
type name is string
function add (const a : int; const b : int) : int is
  a + b
function can_drive (const age : nat) : string is
  if age >= 16n then "yes" else "no"
type middle_name is option (string)
const a_middle_name : middle_name = Some ("Foo")
const no_middle_name : middle_name = None
const age : int = 5
const someAddress : address =
  ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe" : address)
type person is record [
  age  : int;
  name : string
]

const john : person = record [
  age  = 18;
  name = "john doe"
]

const name : string = john.name
type prices is map (nat, mav)

const prices : prices =
  Map.literal (list [
    (10n, 60mumav);
    (50n, 30mumav);
    (100n, 10mumav)
  ])

const price : option (mav) = Map.find_opt (50n, prices)

const prices : prices = Map.update (200n, Some (5mumav), prices)
function fail (const u : unit) : unit is
  failwith ("a failure message")
type animal is
[@layout comb]
| Elephant of unit
| Dog of unit
| Cat of unit
type animal is
[@layout tree]
| Elephant of unit
| Dog of unit
| Cat of unit
module FA0_inferred is {
  type t is unit
  [@entry] function transfer (const _p : unit; const _s : t) : list (operation) * t is
    ((nil : list (operation)), unit)
}
module type FA0_SIG is sig
  type t
  [@entry] const transfer : unit -> t -> list (operation) * t
end
module type FA0_SIG is sig
  type t
  [@entry] const transfer : unit -> t -> list (operation) * t
end

module type FA0Ext_SIG is sig
  include FA0_SIG
  [@entry] const transfer2 : unit -> t -> list (operation) * t
end
module type FA0_SIG is sig
  type t
  [@entry] const transfer : unit -> t -> list (operation) * t
end

module FA0 : FA0_SIG is {
  type t is unit
  [@entry] function transfer (const _p : unit; const _s : t) : list (operation) * t is
    ((nil : list (operation)), unit)
}
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