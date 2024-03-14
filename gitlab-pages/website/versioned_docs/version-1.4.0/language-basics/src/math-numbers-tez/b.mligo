let a : int = 5 - 10

// Subtraction of two nats yields an int
let b : int = 5n - 2n

// Therefore the following is invalid
// let c : nat = 5n - 2n
let d : tez option = 5mumav - 1mumav (* Some (4mumav) *)
let e : tez option = 1mumav - 5mumav (* None *)