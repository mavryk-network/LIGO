function incr_if_true (const b : bool; const n : int) : int is
  block { assert_with_error (b, "My custom error message.") } with n + 1