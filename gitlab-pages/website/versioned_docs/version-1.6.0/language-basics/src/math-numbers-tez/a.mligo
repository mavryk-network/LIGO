// int + int yields int
let a : int = 5 + 10

// nat + int yields int
let b : int = 5n + 10

// mav + mav yields mav
let c : mav = 5mumav + 0.000_010mav

// mav + int or mav + nat is invalid
// let d : mav = 5mumav + 10n

// two nats yield a nat
let e : nat = 5n + 10n

// nat + int yields an int: invalid
// let f : nat = 5n + 10

let g : int = 1_000_000