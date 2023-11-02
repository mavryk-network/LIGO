
let main (_p, s : int * int) : operation list * int =
  let  x : string * int * nat * mav *       string * int =  "foo" , 42  , 24n , 42mav ,        "bar",  42 in
  let _y : mav    * int       * mav * nat * string       = x in
  //       ^^^^^^         ^^^         ^^^            ^^^
  //       changed        removed     added          removed 
  ([] : operation list), s