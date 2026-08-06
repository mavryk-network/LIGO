(* Bitwise and *)
const b_and         : bytes = Bitwise.and (0x0005, 0x0106) (* 0x0004 *)

(* Bitwise or *)
const b_or          : bytes = Bitwise.or (0x0005, 0x0106) (* 0x0107 *)

(* Bitwise xor *)
const b_xor         : bytes = Bitwise.xor (0x0005, 0x0106) (* 0x0103 *)

(* Bitwise shift left *)
const b_shift_left  : bytes = Bitwise.shift_left (0x06, 8n) (* 0x0600 *)

(* Bitwise shift right *)
const b_shift_right : bytes = Bitwise.shift_right (0x0006, 1n) (* 0x0003 *)