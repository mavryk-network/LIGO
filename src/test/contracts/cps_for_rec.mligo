let rec fact (x : int) : int = if (x > 0) then x * fact (x - 1) else 1

let rec zip (type a b) ((xs, ys) : a list * b list) : (a * b) list =
  match xs, ys with
  | [], [] -> []
  | (x :: xs), (y :: ys) -> (x, y) :: (zip (xs, ys))
  | _, _ -> failwith "different length"
