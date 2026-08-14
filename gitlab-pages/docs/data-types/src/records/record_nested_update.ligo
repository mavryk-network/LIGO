type user is record [
  login : string;
  name  : string
]

type account is record [
  user     : user;
  id       : int;
  is_admin : bool
]
function change_login (const login : string; const account : account) : account is
  account with record [user.login = login]