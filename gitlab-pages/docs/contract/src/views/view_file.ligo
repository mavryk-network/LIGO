module C is {
  type storage is string

  [@entry] function append (const a : string; const s : storage) : list (operation) * storage is ((nil : list (operation)), s ^ a)

  [@entry] function clear (const _u : unit; const _s : storage) : list (operation) * storage is ((nil : list (operation)), "")

  function v (const expected_length : nat; const s : storage) : bool is (String.length (s) = expected_length)
}