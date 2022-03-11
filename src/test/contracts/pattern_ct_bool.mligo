let y_mismo (b : bool) = match b, b with
  | true, true -> true
  | true, false -> false
  | false, true -> false
  | false, false -> false
