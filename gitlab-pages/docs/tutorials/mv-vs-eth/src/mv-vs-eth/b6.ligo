const x : option (int) = Some (5)

const x_or_zero : int =
  case x of [
    Some (value) -> value
  | None -> 0
  ]