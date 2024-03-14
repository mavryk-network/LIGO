let add_tez : mav = 21mumav + 0.000_021mav
let sub_tez : mav option = 0.000021mav - 0.000_020mav
let sub_tez_none : mav option = 0.000_020mav - 0.000021mav
let not_enough_tez : mav = 461_168_601_842_738_7903mumav

let add_more_tez : mav =
  100mav + 10mav + 1mav + 0.1mav + 0.01mav + 0.001mav
