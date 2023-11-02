// Example from https://ligolang.org/docs/advanced/entrypoints-contracts/#access-control

const owner : address = ("mv1XJ6kbMgDvXvvtw8KBG2Ne2ngNHxLfuUvE": address);

function main (const action : parameter; const store : storage) : return is
  if Mavryk.source =/= owner then (failwith ("Access denied.") : return)
  else ((nil : list (operation)), store)
