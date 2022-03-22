function sub (const store : tez; const delta : tez) : tez is 
  store - delta

function main (const (_, store) : unit * tez) : list (operation) * tez is
  ((nil : list (operation)), sub (store, 1tez))