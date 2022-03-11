let y_mismo (b : bool) = match b, b with
  | true, true -> true
  | true, false -> true
  | false, true -> true
  | false, false -> false
