type coin is Head of unit | Tail of unit
const head : coin = Head (unit)
const tail : coin = Tail (unit)
type id is nat

type user is
  Admin   of id
| Manager of id
| Guest of unit

const bob : user = Admin (1000n)
const carl : user = Guest (unit)