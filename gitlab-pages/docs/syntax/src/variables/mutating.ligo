function add (const a : int; const b : int) : int is block {
  var c : int := a + b;  // Mutable c is assigned a + b
  c := c + 1             // Reassignment of incremented c
} with c                 // c = a + b + 1