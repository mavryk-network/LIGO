type user is record [
  id       : nat;
  is_admin : bool;
  name     : string
]
const alice : user = record [
  id       = 1n;
  is_admin = True;
  name     = "Alice"
]
const alice_admin : bool = alice.is_admin
function user_to_tuple (const u : user) : nat * bool * string is
  block {
    const record [id; is_admin; name] = u
  } with (id, is_admin, name)
function get_id (const u : user) : nat is
  block {
    const record [id; is_admin = _ia; name = _nm] = u
  } with id