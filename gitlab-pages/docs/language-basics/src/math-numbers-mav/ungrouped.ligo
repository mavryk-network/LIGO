// Bitwise and (first operand can be int or nat)
const four : nat = Bitwise.and (4n, 4n) // 4
const four_ : nat = Bitwise.and (7, 4n) // 4
// Bitwise or
const seven : nat = Bitwise.or (7n, 4n) // 7
// Bitwise xor
const three : nat = Bitwise.xor (7n, 4n) // 3
// Bitwise shift left
const fourteen : nat = Bitwise.shift_left (7n, 1n) // 14
// Bitwise shift right
const seven_ : nat = Bitwise.shift_right (14n, 1n) // 7