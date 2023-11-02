function check_ (const _ : unit) : int is
  {
    var result : int := 0;
    if Mavryk.get_amount() = 100mav then result := 42 else result := 0
  } with result
