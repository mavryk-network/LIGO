type t2 is A of int | B of int

module MyModule is {
  type t5 is A of int | C of bool
  type t4 is A of int | D of int

  module MySubModule is {
    type t6 is A of int | E of mav
  }
}

module MySecondModule is {
  type t3 is A of int | F of int
}

type t1 is A of int | G of mav

// The compiler will search above for sum types with an 'A' constructor
const x = A (42)