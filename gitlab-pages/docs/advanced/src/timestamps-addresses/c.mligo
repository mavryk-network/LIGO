let today : timestamp = Mavryk.get_now ()
let one_day : int = 86400
let in_24_hrs : timestamp = today - one_day
let not_tomorrow : bool = (Mavryk.get_now () = in_24_hrs)