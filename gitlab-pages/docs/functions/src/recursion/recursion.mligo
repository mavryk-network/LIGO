let rec sum (n, acc : int * int) : int =
  if n < 1 then acc else sum (n-1, acc + n)

let rec fibonacci (n, n_1, n_0 : int * int * int) : int =
  if n < 2 then n_1 else fibonacci (n-1, n_1 + n_0, n_1)