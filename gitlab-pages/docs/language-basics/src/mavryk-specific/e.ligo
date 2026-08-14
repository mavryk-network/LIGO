const origination : operation * address = Mavryk.create_contract (
  (function (const p : nat; const s : string) is
    ((nil : list (operation)), s)),
  (None : option (key_hash)),
  3mav,
  "initial_storage"
)