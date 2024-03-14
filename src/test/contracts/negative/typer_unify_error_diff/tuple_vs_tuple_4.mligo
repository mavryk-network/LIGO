
let main (_p : int) (s : int) : operation list * int =
  let x  : int *                nat * int * nat     * int *       nat = 42 , 4n , 42 , 24n , 42 , 24n in
  let _y : int * mav * string * nat * int * address * int * mav * nat = x in
//               ^^^   ^^^^^^               ^^^^^^^         ^^^
//               added added                changed         added                 
  ([] : operation list), s