let rec foo (n : int) (x : string) : string = if n = 0 then x else foo (n - 1) (String.concat x "toto")

let main ((n, s) : int * string) : operation list * string =
  ([] : operation list), foo n s
