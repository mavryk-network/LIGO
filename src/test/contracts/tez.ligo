const add_mav : mav = 21mumav + 0.000_021mav

const sub_mav : option(mav) = 21mumav - 20mumav
const sub_mav_none : option(mav) = 20mumav - 21mumav

(* This is not enough. *)

const not_enough_mav : mav = 461_168_601_842_738_7903mumav

const nat_mul_mav : mav = 1n * 100mumav
const tez_mul_nat : mav = 100mumav * 10n

const tez_div_mav1 : nat = 100mumav / 1mumav
const tez_div_mav2 : nat = 100mumav / 90mumav
const tez_div_mav3 : nat = 100mumav / 110mumav

const tez_mod_mav1 : mav = 100mumav mod 1mumav
const tez_mod_mav2 : mav = 100mumav mod 90mumav
const tez_mod_mav3 : mav = 100mumav mod 110mumav
