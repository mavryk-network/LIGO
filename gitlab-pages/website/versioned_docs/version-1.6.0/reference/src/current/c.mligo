let today     = Mavryk.get_now ()
let one_day   = 86_400
let in_24_hrs = today - one_day
let not_tomorrow = (Mavryk.get_now () = in_24_hrs)