module M is {
  type t1 is A of int | B of int
}
module M is {
  const y = 10
}

// This will fail because A will not be found
// const x = A (42)