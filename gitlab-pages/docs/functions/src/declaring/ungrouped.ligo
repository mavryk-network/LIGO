function drop (const x : int; const _ : int) : int is x
function drop (const x : int; const _y : int) : int is x  // _y silently ignored
function convoluted_doubling (const x : int) : int is
  block {
    function add_x (const y : int) : int is x + y  // x is bound by convoluted_doubling
  } with add_x (x)