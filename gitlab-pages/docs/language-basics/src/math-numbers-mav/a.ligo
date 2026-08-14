// int + int yields int
const a : int = 5 + 10

// nat + int yields int
const b : int = 5n + 10

// mav + mav yields mav
const c : mav = 5mumav + 0.000_010mav

// mav + int or mav + nat is invalid
// const d : mav = 5mumav + 10n

// two nats yield a nat
const e : nat = 5n + 10n

// nat + int yields an int: invalid
// const f : nat = 5n + 10

const g : int = 1_000_000