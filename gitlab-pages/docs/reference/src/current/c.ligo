const today     = Mavryk.get_now ()
const one_day   = 86_400
const in_24_hrs = today - one_day
const not_tomorrow = (Mavryk.get_now () = in_24_hrs)