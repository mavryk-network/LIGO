let foo (x : nat) = x

// Errors. Should report: Expected: "nat", but got: "int"
let bar : nat = foo 1