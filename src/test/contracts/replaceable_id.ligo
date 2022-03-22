// storage type
type storage_t is address

// entry points parameter types
type change_addr_pt is address

type message_t is list (operation)
type pass_message_pt is unit -> message_t

type contract_return_t is list (operation) * storage_t

type entry_point_t is
  Change_address of change_addr_pt
| Pass_message   of pass_message_pt

function change_address (const (param, s) : change_addr_pt * storage_t) : contract_return_t is
  block {
    if sender =/= s then failwith ("Unauthorized sender")
    else skip
  } with ((nil : list (operation)), param)

function pass_message (const (param, s) : pass_message_pt * storage_t ) : contract_return_t is
  block {
    if sender =/= s then failwith("Unauthorized sender") else skip;
    var _message : pass_message_pt := param
  } with (param (unit), s)

function main (const (param, s) : entry_point_t * storage_t) :
  contract_return_t is
  case param of [
    Change_address (p) -> change_address((p,s))
  | Pass_message (p)   -> pass_message((p,s))
  ]
