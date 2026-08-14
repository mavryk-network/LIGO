module type Euro_SIG is sig
  type t
  const add : t -> t -> t
  const one : t
  const two : t
end
module Euro : Euro_SIG is {
  type t is nat // No more abstract
  function add (const a : t; const b : t) : t is a + b
  const one : t = 1n
  const two : t = 2n
}