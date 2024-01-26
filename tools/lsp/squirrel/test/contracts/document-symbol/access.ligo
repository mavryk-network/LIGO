// Example from https://ligo.mavryk.org/docs/advanced/entrypoints-contracts/#access-control

const owner : address = ("mv18Cw7psUrAAPBpXYd9CtCpHg9EgjHP9KTe": address);

function main (const action : parameter; const store : storage) : return is
  if Mavryk.source =/= owner then (failwith ("Access denied.") : return)
  else ((nil : list (operation)), store)
