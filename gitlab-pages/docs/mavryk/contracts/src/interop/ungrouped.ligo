type animal is
[@layout comb]
| Elephant of unit
| Dog of unit
| Cat of unit
type artist is
  [@layout comb] record [
  genre : string;
  since : timestamp;
  name : string
]
type w_and_v is michelson_pair (int, "w", nat, "v")
type x_and is michelson_pair (string, "x", w_and_v, "other")
type y_or is michelson_or (unit, "y", x_and, "other")
type z_or is michelson_or (unit, "z", y_or, "other")
const z : z_or = (M_left (unit) : z_or)

const y_1 : y_or = (M_left (unit) : y_or)
const y : z_or = (M_right (y_1) : z_or)

const x_pair : x_and = ("foo", (2, 3n))
const x_1 : y_or = (M_right (x_pair) : y_or)
const x : z_or = (M_right (y_1) : z_or)
type storage is int

type parameter is
 | Left of int
 | Right of int

[@entry]
function main (const p : parameter; const x : storage) : list (operation) * storage is
  ((nil : list (operation)),
   case p of [
     Left (i) -> x - i
   | Right (i) -> x + i
   ])