type storage_t is timestamp

type message_t is unit -> list (operation)
type default_pt is unit
type call_pt is message_t
type contract_return_t is list (operation) * storage_t

type entry_point_t is
| Call of call_pt
| Default of default_pt

function call (const (p, s) : call_pt * storage_t) : contract_return_t is
  block {
    if s >= now then failwith ("Contract is still time locked") else skip;
    const message : message_t = p;
    const ret_ops : list (operation) = message (unit)
  } with (ret_ops, s)

function default (const (_, s) : default_pt * storage_t) :
  contract_return_t is
  ((nil : list (operation)), s)

function main(const (param, s) : entry_point_t * storage_t) :
  contract_return_t is
  case param of [
    Call    (p) -> call((p,s))
  | Default (p) -> default((p,s))
  ]
