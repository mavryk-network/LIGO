type user is record [
  login : string;
  name  : string
]

type account is record [
  user     : user;
  id       : int;
  is_admin : bool
]

const user : user = record [login = "al"; name = "Alice"]
const alice : account = record [user; id = 5; is_admin = True]
const is_alice_admin : bool = alice.is_admin // = true
function user_to_triple (const a : account) : user * int * bool is
  block {
    var record [ user; id; is_admin ] := a
  } with (user, id, is_admin)
function get_id (const a : account) : int is
  block {
    var record [ user = _u; id; is_admin = _ia ] := a  // To avoid a warning
  } with id