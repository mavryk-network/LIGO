type storage is int
type result is list (operation) * storage

[@entry] [@no_mutation]
function sub (const delta : int; const store : storage) : result is
  ((nil : list (operation)), store - delta)