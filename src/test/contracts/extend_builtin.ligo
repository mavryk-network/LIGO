module Mavryk is {
  const x = 42
  function f (const x  : int) : int is x + 2
}

const y = Mavryk.f(Mavryk.x)
