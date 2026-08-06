type id is nat

type user is
  Admin   of id
| Manager of id
| Guest of unit

const u : user = Admin (1000n)
const g : user = Guest (unit)