function weird_length (const v : list (int)) : int is
  case v of [
    nil -> -1
  | a # (b # (c # nil)) -> -2
  | x -> int (List.length (x))
  ]