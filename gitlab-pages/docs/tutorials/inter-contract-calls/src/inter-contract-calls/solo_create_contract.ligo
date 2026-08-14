const op = Mavryk.create_contract (
  function (const p : int; const s : int) : list (operation) * int is
    ((nil : list (operation)), p + s),
  (None : option (key_hash)),
  0mumav,
  1
)