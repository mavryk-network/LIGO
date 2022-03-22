// Test while loops in PascaLIGO

recursive function sum (const (n, acc) : int * int) : int is
  if n<1 then acc else sum((n-1,acc+n))

recursive function fibo (const (n, n_1, n_0) : int * int * int) : int is
  if n<2 then n_1 else fibo((n-1,n_1+n_0,n_1))
