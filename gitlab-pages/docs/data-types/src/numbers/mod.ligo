const a : nat = 120  mod 9;  // int mod int yields nat
const b : nat = 120n mod 9;  // nat mod int yields nat
const c : nat = 120n mod 9n; // nat mod nat yields nat
const d : nat = 120  mod 9n; // int mod nat yields nat