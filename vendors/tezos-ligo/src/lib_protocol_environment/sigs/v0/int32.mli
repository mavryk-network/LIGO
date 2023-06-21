(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* TEZOS CHANGES

   * Import version 4.06.1
   * Remove deprecated functions
*)

(** 32-bit integers.

    This module provides operations on the type [int32]
    of signed 32-bit integers.  Unlike the built-in [int] type,
    the type [int32] is guaranteed to be exactly 32-bit wide on all
    platforms.  All arithmetic operations over [int32] are taken
    modulo 2{^32}.

    Performance notice: values of type [int32] occupy more memory
    space than values of type [int], and arithmetic operations on
    [int32] are generally slower than those on [int].  Use [int32]
    only when the application requires exact 32-bit arithmetic. *)

(** The 32-bit integer 0. *)
val zero : int32

(** The 32-bit integer 1. *)
val one : int32

(** The 32-bit integer -1. *)
val minus_one : int32

(** Unary negation. *)
external neg : int32 -> int32 = "%int32_neg"

(** Addition. *)
external add : int32 -> int32 -> int32 = "%int32_add"

(** Subtraction. *)
external sub : int32 -> int32 -> int32 = "%int32_sub"

(** Multiplication. *)
external mul : int32 -> int32 -> int32 = "%int32_mul"

(** Integer division.  Raise [Division_by_zero] if the second
    argument is zero.  This division rounds the real quotient of
    its arguments towards zero, as specified for {!Pervasives.(/)}. *)
external div : int32 -> int32 -> int32 = "%int32_div"

(** Integer remainder.  If [y] is not zero, the result
    of [Int32.rem x y] satisfies the following property:
    [x = Int32.add (Int32.mul (Int32.div x y) y) (Int32.rem x y)].
    If [y = 0], [Int32.rem x y] raises [Division_by_zero]. *)
external rem : int32 -> int32 -> int32 = "%int32_mod"

(** Successor.  [Int32.succ x] is [Int32.add x Int32.one]. *)
val succ : int32 -> int32

(** Predecessor.  [Int32.pred x] is [Int32.sub x Int32.one]. *)
val pred : int32 -> int32

(** Return the absolute value of its argument. *)
val abs : int32 -> int32

(** The greatest representable 32-bit integer, 2{^31} - 1. *)
val max_int : int32

(** The smallest representable 32-bit integer, -2{^31}. *)
val min_int : int32

(** Bitwise logical and. *)
external logand : int32 -> int32 -> int32 = "%int32_and"

(** Bitwise logical or. *)
external logor : int32 -> int32 -> int32 = "%int32_or"

(** Bitwise logical exclusive or. *)
external logxor : int32 -> int32 -> int32 = "%int32_xor"

(** Bitwise logical negation. *)
val lognot : int32 -> int32

(** [Int32.shift_left x y] shifts [x] to the left by [y] bits.
    The result is unspecified if [y < 0] or [y >= 32]. *)
external shift_left : int32 -> int -> int32 = "%int32_lsl"

(** [Int32.shift_right x y] shifts [x] to the right by [y] bits.
    This is an arithmetic shift: the sign bit of [x] is replicated
    and inserted in the vacated bits.
    The result is unspecified if [y < 0] or [y >= 32]. *)
external shift_right : int32 -> int -> int32 = "%int32_asr"

(** [Int32.shift_right_logical x y] shifts [x] to the right by [y] bits.
    This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of [x].
    The result is unspecified if [y < 0] or [y >= 32]. *)
external shift_right_logical : int32 -> int -> int32 = "%int32_lsr"

(** Convert the given integer (type [int]) to a 32-bit integer
    (type [int32]). *)
external of_int : int -> int32 = "%int32_of_int"

(** Convert the given 32-bit integer (type [int32]) to an
    integer (type [int]).  On 32-bit platforms, the 32-bit integer
    is taken modulo 2{^31}, i.e. the high-order bit is lost
    during the conversion.  On 64-bit platforms, the conversion
    is exact. *)
external to_int : int32 -> int = "%int32_to_int"

(** Convert the given floating-point number to a 32-bit integer,
    discarding the fractional part (truncate towards 0).
    The result of the conversion is undefined if, after truncation,
    the number is outside the range \[{!Int32.min_int}, {!Int32.max_int}\]. *)
external of_float : float -> int32
  = "caml_int32_of_float" "caml_int32_of_float_unboxed"
  [@@unboxed] [@@noalloc]

(** Convert the given 32-bit integer to a floating-point number. *)
external to_float : int32 -> float
  = "caml_int32_to_float" "caml_int32_to_float_unboxed"
  [@@unboxed] [@@noalloc]

(** Convert the given string to a 32-bit integer.
    The string is read in decimal (by default, or if the string
    begins with [0u]) or in hexadecimal, octal or binary if the
    string begins with [0x], [0o] or [0b] respectively.

    The [0u] prefix reads the input as an unsigned integer in the range
    [[0, 2*Int32.max_int+1]].  If the input exceeds {!Int32.max_int}
    it is converted to the signed integer
    [Int32.min_int + input - Int32.max_int - 1].

    The [_] (underscore) character can appear anywhere in the string
    and is ignored.
    Raise [Failure "Int32.of_string"] if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [int32]. *)
external of_string : string -> int32 = "caml_int32_of_string"

(** Same as [of_string], but return [None] instead of raising.
    @since 4.05 *)
val of_string_opt : string -> int32 option

(** Return the string representation of its argument, in signed decimal. *)
val to_string : int32 -> string

(** Return the internal representation of the given float according
    to the IEEE 754 floating-point 'single format' bit layout.
    Bit 31 of the result represents the sign of the float;
    bits 30 to 23 represent the (biased) exponent; bits 22 to 0
    represent the mantissa. *)
external bits_of_float : float -> int32
  = "caml_int32_bits_of_float" "caml_int32_bits_of_float_unboxed"
  [@@unboxed] [@@noalloc]

(** Return the floating-point number whose internal representation,
    according to the IEEE 754 floating-point 'single format' bit layout,
    is the given [int32]. *)
external float_of_bits : int32 -> float
  = "caml_int32_float_of_bits" "caml_int32_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]

(** An alias for the type of 32-bit integers. *)
type t = int32

(** The comparison function for 32-bit integers, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [Int32] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)
val compare : t -> t -> int

(** The equal function for int32s.
    @since 4.03.0 *)
val equal : t -> t -> bool
