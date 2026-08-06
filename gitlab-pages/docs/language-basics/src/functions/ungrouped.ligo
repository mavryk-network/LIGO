function k (const x : int; const _ : int) : int is x
function k (const x : int; const _y : int) : int is x
function closure_example (const i : int) : int is {
  function closure (const j : int) : int is i + j
} with closure (i)