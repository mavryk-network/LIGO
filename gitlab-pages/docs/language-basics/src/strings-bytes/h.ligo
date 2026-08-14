(* bytes -> nat *)
const test_bytes_nat : nat = nat (0x1234) (* 1234n *)

(* nat -> bytes *)
const test_nat_bytes : bytes = bytes (4660n) (* 0x1234 *)
(* bytes -> int *)
const test_bytes_int : int = int (0x1234) (* 4660 *)

(* int -> bytes *)
const test_int_bytes : bytes = bytes (4660) (* 0x1234 *)