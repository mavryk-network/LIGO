module type Euro_SIG is sig
  type t
  const add : t * t -> t
  const one : t
  const two : t
end

module type NewEuro_SIG is sig
  include Euro_SIG
  const ten : t
end