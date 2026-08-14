type int_option is
  Number of int
| Null of unit

const x : int_option = Number (5)

const y : int_option = Null (unit)