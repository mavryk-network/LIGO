let main (parameter, storage : int * unit) =
  if parameter < 100 then ([] : operation list), ()
  else (Mavryk.failwith "PARAM_TOO_LARGE" : operation list * unit)
