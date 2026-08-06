function incr_if_true (const b : bool; const n : int) : int is
  block { assert (b) } with n + 1

function incr_if_some (const b : option (unit); const n : int) : int is
  block { assert_some (b) } with n + 1